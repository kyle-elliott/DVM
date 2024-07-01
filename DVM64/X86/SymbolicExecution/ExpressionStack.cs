using System.Collections;
using System.Numerics;
using System.Runtime.InteropServices;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64.X86.SymbolicExecution;

/// <summary>
/// The ExpressionStack class represents a stack of expressions in symbolic execution.
/// </summary>
public class ExpressionStack(Context ctx, BitVecNum stackPointer) : IEnumerable<BitVecExpr>
{
    /// <summary>
    /// The current stack pointer.
    /// </summary>
    public BitVecNum StackPointer = stackPointer;

    /// <summary>
    /// The initial stack pointer.
    /// </summary>
    private readonly BitVecNum _initialStackPointer = stackPointer;

    private const int RedZoneOffset = 5;

    // Stack state represented as a list of expressions.
    private readonly List<BitVecExpr> _stack =
    [
        ctx.MkBVConst("red_zone_0", 64),
        ctx.MkBVConst("red_zone_1", 64),
        ctx.MkBVConst("red_zone_2", 64),
        ctx.MkBVConst("red_zone_3", 64),
        ctx.MkBVConst("init_ret_addr", 64)
    ];

    /// <summary>
    /// Pushes an expression onto the stack.
    /// </summary>
    /// <param name="expr">The expression to push onto the stack.</param>
    public void PushExpression(BitVecExpr expr)
    {
        if (expr.SortSize != 64)
            throw new NotImplementedException("Currently only supporting 64bit stack pushes!");

        StackPointer = (BitVecNum)ctx.MkBVSub(StackPointer, ctx.MkBV(8, 64)).Simplify();

        var num = (BitVecNum)ctx.MkBVSub(_initialStackPointer, StackPointer).Simplify();
        var (quotient, remainder) = BigInteger.DivRem(num.BigInteger, 8);

        if (remainder != 0)
            throw new NotImplementedException("Only support pushing on 8 byte aligned addresses!");

        if ((int)quotient + RedZoneOffset > _stack.Count)
        {
            _stack.Add(expr);
        }
        else
        {
            _stack[(int)quotient + RedZoneOffset - 1] = expr;
        }
    }

    /// <summary>
    /// Pops an expression from the stack.
    /// </summary>
    /// <returns>The popped expression.</returns>
    public BitVecExpr PopExpression()
    {
        var num = (BitVecNum)ctx.MkBVSub(_initialStackPointer, StackPointer).Simplify();
        var (quotient, remainder) = BigInteger.DivRem(num.BigInteger, 8);

        if (remainder != 0)
            throw new NotImplementedException("Only support pushing on 8 byte aligned addresses!");

        var expr = _stack[(int)quotient + RedZoneOffset - 1];

        StackPointer = (BitVecNum)ctx.MkBVAdd(StackPointer, ctx.MkBV(8, 64)).Simplify();
        return expr;
    }

    /// <summary>
    /// Stores an expression at a given index in the stack.
    /// </summary>
    /// <param name="indexExpr">The index at which to store the expression.</param>
    /// <param name="expr">The expression to store.</param>
    /// <param name="size">The size of the memory to store the expression in.</param>
    public void StoreExpression(BitVecNum indexExpr, BitVecExpr expr, MemorySize size)
    {
        var num = (BitVecNum)ctx.MkBVSub(_initialStackPointer, indexExpr).Simplify();
        var (quotient, remainder) = Math.DivRem(num.UInt, 8);

        if (remainder > 0)
        {
            quotient++;
            remainder = 8 - remainder;
        }

        var memorySize = size switch
        {
            MemorySize.Int8 or MemorySize.UInt8 => 8u,
            MemorySize.Int16 or MemorySize.UInt16 => 16u,
            MemorySize.Int32 or MemorySize.UInt32 => 32u,
            MemorySize.Int64 or MemorySize.UInt64 => 64u,
            _ => throw new ArgumentOutOfRangeException(nameof(size), size, null)
        };

        var lowBit = remainder * 8;

        var highBit = lowBit + memorySize;

        var elementFromStack = CollectionsMarshal.AsSpan(_stack)[(int)quotient + RedZoneOffset - 1];

        if (highBit > 64)
        {
            var nextElemFromStack = _stack[(int)quotient + RedZoneOffset - 2];
            elementFromStack = (BitVecExpr)ctx.MkConcat(nextElemFromStack, elementFromStack).Simplify();
        }

        var storedExpr = expr;

        BitVecExpr? highPart = null;
        if (highBit != 64)
            highPart = (BitVecExpr)ctx.MkExtract(elementFromStack.SortSize - 1, highBit, elementFromStack).Simplify();

        BitVecExpr? lowPart = null;
        if (lowBit != 0)
            lowPart = (BitVecExpr)ctx.MkExtract(lowBit - 1, 0, elementFromStack).Simplify();

        if (storedExpr.SortSize != 64 || remainder != 0)
        {
            if (highPart != null)
                storedExpr = (BitVecExpr)ctx.MkConcat(highPart, storedExpr).Simplify();

            if (lowPart != null)
                storedExpr = (BitVecExpr)ctx.MkConcat(storedExpr, lowPart).Simplify();
        }

        // If the storedExpr still isn't 64 bits, we need to zero extend it to 64 bits
        if (storedExpr.SortSize < 64)
            storedExpr = (BitVecExpr)ctx.MkZeroExt(64 - storedExpr.SortSize, storedExpr).Simplify();

        switch (storedExpr.SortSize)
        {
            case 64:
            {
                CollectionsMarshal.AsSpan(_stack)[(int)quotient + RedZoneOffset - 1] = storedExpr;
                break;
            }

            case 128:
            {
                var high = (BitVecExpr)ctx.MkExtract(127, 64, storedExpr).Simplify();
                var low = (BitVecExpr)ctx.MkExtract(63, 0, storedExpr).Simplify();
                CollectionsMarshal.AsSpan(_stack)[(int)quotient + RedZoneOffset - 2] = high;
                CollectionsMarshal.AsSpan(_stack)[(int)quotient + RedZoneOffset - 1] = low;

                break;
            }
        }
    }

    /// <summary>
    /// Loads an expression from a given index in the stack.
    /// </summary>
    /// <param name="indexExpr">The index from which to load the expression.</param>
    /// <param name="size">The size of the memory from which to load the expression.</param>
    /// <returns>The loaded expression.</returns>
    public BitVecExpr LoadExpression(BitVecNum indexExpr, MemorySize size)
    {
        var num = (BitVecNum)ctx.MkBVSub(_initialStackPointer, indexExpr).Simplify();
        var (quotient, remainder) = Math.DivRem(num.Int, 8);

        if (remainder > 0)
        {
            quotient++;
            remainder = 8 - remainder;
        }

        var memorySize = size switch
        {
            MemorySize.Int8 or MemorySize.UInt8 => 8u,
            MemorySize.Int16 or MemorySize.UInt16 => 16u,
            MemorySize.Int32 or MemorySize.UInt32 => 32u,
            MemorySize.Int64 or MemorySize.UInt64 => 64u,
            _ => throw new ArgumentOutOfRangeException(nameof(size), size, null)
        };

        var lowBit = (uint)remainder * 8;

        var highBit = lowBit + memorySize;

        var stackExpr = _stack[quotient + RedZoneOffset - 1];

        if (highBit > 64)
        {
            stackExpr = (BitVecExpr)ctx.MkConcat(_stack[quotient + RedZoneOffset - 2], stackExpr).Simplify();
        }

        return (BitVecExpr)ctx.MkExtract(highBit - 1, lowBit, stackExpr).Simplify();
    }

    /// <summary>
    /// Checks if the given index is a stack access.
    /// </summary>
    /// <param name="indexExpr">The index to check.</param>
    /// <returns>True if the index is a stack access, false otherwise.</returns>
    public bool IsStackAccess(BitVecExpr indexExpr)
    {
        if (indexExpr is not BitVecNum)
            return false;

        var num = (BitVecNum)ctx.MkBVSub(_initialStackPointer, indexExpr).Simplify();
        var quotient = num.UInt64 / 8;

        return quotient <= (ulong)_stack.Count;
    }

    /// <summary>
    /// Simplifies all expressions in the stack.
    /// </summary>
    public void Simplify()
    {
        foreach (ref var expr in CollectionsMarshal.AsSpan(_stack))
            expr = (BitVecExpr)expr.Simplify();
    }

    /// <summary>
    /// Aligns the stack to 64-bit values.
    /// </summary>
    public void Align()
    {
        if (StackPointer is not { IsBV: true, IsNumeral: true } num)
            return;

        num = (BitVecNum)ctx.MkBVSub(_initialStackPointer, num).Simplify();
        var (quotient, remainder) = Math.DivRem(num.UInt64, 8);

        if (remainder != 0)
            throw new NotImplementedException("Can only align stack to 64bit values!");

        while (quotient > (ulong)_stack.Count - RedZoneOffset)
            _stack.Add(ctx.MkBV(0, 64));
    }

    /// <summary>
    /// Returns an enumerator that iterates through the stack.
    /// </summary>
    /// <returns>An enumerator that can be used to iterate through the stack.</returns>
    public IEnumerator<BitVecExpr> GetEnumerator()
    {
        return _stack.GetEnumerator();
    }

    /// <summary>
    /// Returns an enumerator that iterates through the stack.
    /// </summary>
    /// <returns>An enumerator that can be used to iterate through the stack.</returns>
    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}