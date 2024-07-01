using DVM64.Extensions;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64.X86.SymbolicExecution;

public class InstructionTranslate(PositionCodeReader reader, Context ctx)
{
    private (BitVecExpr, ContextKind) ConcretizeOperand(Instruction instruction, int opIndex,
        ref RegisterContext state, bool requestAddress = false)
    {
        var kind = instruction.GetOpKind(opIndex);
        var register = instruction.GetOpRegister(opIndex);

        switch (kind)
        {
            case OpKind.Register:
                return state.IcedRegisterToContextRegister(ctx, register);

            case OpKind.Immediate8:
            case OpKind.Immediate8_2nd:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return (ctx.MkBV(immediate, 8), ContextKind.Immediate);
            }

            case OpKind.Immediate16:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return (ctx.MkBV(immediate, 16), ContextKind.Immediate);
            }

            case OpKind.Immediate8to16:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return ((BitVecExpr)ctx.MkSignExt(8, ctx.MkBV(immediate, 8)).Simplify(), ContextKind.Immediate);
            }

            case OpKind.Immediate32:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return (ctx.MkBV(immediate, 32), ContextKind.Immediate);
            }

            case OpKind.Immediate8to32:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return ((BitVecExpr)ctx.MkSignExt(24, ctx.MkBV(immediate, 8)).Simplify(), ContextKind.Immediate);
            }

            case OpKind.Immediate64:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return (ctx.MkBV(immediate, 64), ContextKind.Immediate);
            }

            case OpKind.Immediate8to64:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return ((BitVecExpr)ctx.MkSignExt(56, ctx.MkBV(immediate, 8)).Simplify(), ContextKind.Immediate);
            }

            case OpKind.Immediate32to64:
            {
                var immediate = instruction.GetImmediate(opIndex);
                return ((BitVecExpr)ctx.MkSignExt(32, ctx.MkBV(immediate, 32)).Simplify(), ContextKind.Immediate);
            }

            case OpKind.Memory:
            {
                BitVecExpr? baseExpression = null;
                if (instruction.MemoryBase != Register.None && instruction.MemoryBase != Register.RIP)
                {
                    baseExpression =
                        state.IcedRegisterToContextRegister(ctx, instruction.MemoryBase).Item1;
                }

                BitVecExpr? indexExpression = null;
                if (instruction.MemoryIndex != Register.None)
                    indexExpression =
                        state.IcedRegisterToContextRegister(ctx, instruction.MemoryIndex).Item1;

                BitVecExpr? indexScaleExpression = null;
                if (instruction.MemoryIndexScale != 0)
                    indexScaleExpression = ctx.MkBV(instruction.MemoryIndexScale, 64);

                BitVecExpr? dispExpression = null;
                if (instruction.MemoryDisplSize != 0)
                    dispExpression = ctx.MkBV(instruction.MemoryDisplacement64, 64);

                BitVecExpr? sibExpression;
                if (indexScaleExpression != null && indexExpression != null)
                    sibExpression = ctx.MkBVMul(indexScaleExpression, indexExpression);
                else if (indexExpression != null)
                    sibExpression = indexExpression;
                else
                    sibExpression = ctx.MkBV(0, 64);

                if (baseExpression != null)
                    sibExpression = ctx.MkBVAdd(sibExpression, baseExpression);

                if (dispExpression != null)
                    sibExpression = ctx.MkBVAdd(sibExpression, dispExpression);

                if (sibExpression == null)
                    throw new InvalidOperationException(
                        $"Unable to build an expression for memory access on {instruction.ToString()}!");

                var unsimplified = sibExpression;
                sibExpression = (BitVecExpr)sibExpression.Simplify();

                if (requestAddress)
                {
                    return (sibExpression, ContextKind.Memory);
                }

                if (state.Stack.IsStackAccess(sibExpression))
                {
                    return (state.Stack.LoadExpression((BitVecNum)sibExpression, instruction.MemorySize),
                        ContextKind.Memory);
                }

                if (sibExpression is BitVecNum num &&
                    reader.VirtualAddressInRange(num.UInt64)) // Expression was not a stack read, load memory
                {
                    var count = instruction.MemorySize switch
                    {
                        MemorySize.Int8 or MemorySize.UInt8 => 1u,
                        MemorySize.Int16 or MemorySize.UInt16 => 2u,
                        MemorySize.Int32 or MemorySize.UInt32 => 4u,
                        MemorySize.Int64 or MemorySize.UInt64 => 8u,
                        _ => throw new ArgumentOutOfRangeException(nameof(instruction.MemorySize),
                            instruction.MemorySize, null)
                    };

                    reader.Position = (int)(uint)num.UInt64;

                    ulong res = 0;
                    for (var i = 0; i < count; i++)
                        res |= (ulong)reader.ReadByte() << (i * 8);

                    return (ctx.MkBV(res, count * 8), ContextKind.Memory);
                }

                if (instruction is { HasSegmentPrefix: true, SegmentPrefix: Register.FS or Register.GS })
                {
                    return (ctx.MkBVConst($"{instruction.SegmentPrefix}:[{sibExpression}]", 64), ContextKind.Memory);
                }

                throw new NotImplementedException($"Unable to concretize memory load: {unsimplified}");
            }

            default:
                throw new ArgumentOutOfRangeException(nameof(kind), kind,
                    $"Instruction kind {kind.ToString()} is not handled!");
        }
    }

    private void StoreExpr(BitVecExpr storeExpr, ContextKind destKind, ref RegisterContext state,
        Register? storeReg = null, BitVecExpr? memAddr = null, MemorySize? memorySize = null)
    {
        switch (destKind)
        {
            case ContextKind.Register64:
            case ContextKind.RegisterL32:
            case ContextKind.RegisterL16:
            case ContextKind.RegisterH8:
            case ContextKind.RegisterL8:
            {
                ArgumentNullException.ThrowIfNull(storeReg);

                state.StoreExprInContextRegister(ctx, storeExpr, storeReg.Value, destKind);
                return;
            }

            case ContextKind.Memory:
            {
                ArgumentNullException.ThrowIfNull(memAddr);
                ArgumentNullException.ThrowIfNull(memorySize);

                if (state.Stack.IsStackAccess(memAddr))
                {
                    state.Stack.StoreExpression((BitVecNum)memAddr, storeExpr, memorySize.Value);
                    return;
                }

                if (memAddr is BitVecNum num && reader.VirtualAddressInRange(num.UInt64))
                {
                    throw new NotImplementedException($"Unable to write to arbitrary mem addr: {num}");
                }

                throw new NotImplementedException($"memAddr wasn't stack or within file range: {memAddr}");
            }

            default:
                throw new ArgumentOutOfRangeException(nameof(destKind), destKind, null);
        }
    }

    private void TranslateMov(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        switch (instruction.Mnemonic)
        {
            case Mnemonic.Movsx:
            case Mnemonic.Movsxd:
            {
                e2 = ctx.MkSignExt(e1.SortSize - e2.SortSize, e2);
                break;
            }

            case Mnemonic.Movzx:
            {
                e2 = ctx.MkZeroExt(e1.SortSize - e2.SortSize, e2);
                break;
            }

            case Mnemonic.Mov:
                break;

            default:
                throw new ArgumentOutOfRangeException(nameof(instruction.Mnemonic), instruction.Mnemonic,
                    $"Instruction mnemonic {instruction.Mnemonic} is not handled in TranslateMov!");
        }

        StoreExpr(e2, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
    }

    private void TranslateCmov(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var conExpr = instruction.Mnemonic switch
        {
            Mnemonic.Cmova => ctx.MkAnd(ctx.MkNot(state.Cf), ctx.MkNot(state.Zf)),
            Mnemonic.Cmovae => ctx.MkNot(state.Cf),
            Mnemonic.Cmovb => state.Cf,
            Mnemonic.Cmovbe => ctx.MkOr(state.Cf, state.Zf),
            Mnemonic.Cmove => state.Zf,
            Mnemonic.Cmovg => ctx.MkAnd(ctx.MkNot(state.Zf), ctx.MkEq(state.Sf, state.Of)),
            Mnemonic.Cmovge => ctx.MkEq(state.Sf, state.Of),
            Mnemonic.Cmovl => ctx.MkXor(state.Sf, state.Of),
            Mnemonic.Cmovle => ctx.MkOr(state.Zf, ctx.MkXor(state.Sf, state.Of)),
            Mnemonic.Cmovne => ctx.MkNot(state.Zf),
            Mnemonic.Cmovno => ctx.MkNot(state.Of),
            Mnemonic.Cmovnp => ctx.MkNot(state.Pf),
            Mnemonic.Cmovns => ctx.MkNot(state.Sf),
            Mnemonic.Cmovo => state.Of,
            Mnemonic.Cmovp => state.Pf,
            Mnemonic.Cmovs => state.Sf,
            _ => throw new ArgumentOutOfRangeException(nameof(instruction.Mnemonic), $"{instruction.Mnemonic}", null)
        };

        var resExpr = (BitVecExpr)ctx.MkITE(conExpr, e2, e1).Simplify();

        StoreExpr(resExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
    }

    private void TranslatePush(Instruction instruction, ref RegisterContext state)
    {
        if (instruction.OpCode.OperandSize == 64)
        {
            var (e1, _) = ConcretizeOperand(instruction, 0, ref state, true);

            if (instruction.Op0Kind == OpKind.Register || instruction.Op0Kind.IsImmediate())
                state.Stack.PushExpression(e1);
            else if (instruction.Op0Kind == OpKind.Memory)
            {
                if (state.Stack.IsStackAccess(e1))
                {
                    state.Stack.PushExpression(state.Stack.LoadExpression((BitVecNum)e1, instruction.MemorySize));
                }
                else
                    throw new NotImplementedException(
                        $"{instruction.Mnemonic} does not support {instruction.MemoryBase} MemoryBase!");
            }
            else
                throw new NotImplementedException(
                    $"{instruction.Mnemonic} does not support {instruction.Op0Kind} OpKind!");
        }
        else
        {
            throw new NotImplementedException(
                $"{instruction.Mnemonic} does not support {instruction.OpCount} OpCount or {instruction.OpCode.OperandSize} Operand Size!");
        }
    }

    private void TranslatePop(Instruction instruction, ref RegisterContext state)
    {
        if (instruction.OpCode.OperandSize == 64)
        {
            var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state, true);

            if (instruction.Op0Kind == OpKind.Register)
                state.StoreExprInContextRegister(ctx, state.Stack.PopExpression(), instruction.Op0Register, destKind);
            else if (instruction.Op0Kind == OpKind.Memory)
            {
                if (state.Stack.IsStackAccess(e1))
                {
                    var expr = state.Stack.LoadExpression((BitVecNum)e1, instruction.MemorySize);
                    state.Stack.PopExpression();
                    state.Stack.StoreExpression(state.Stack.StackPointer, expr, instruction.MemorySize);
                }
                else
                    throw new NotImplementedException(
                        $"{instruction.Mnemonic} does not support {instruction.MemoryBase} MemoryBase!");
            }
        }
        else
        {
            throw new NotImplementedException(
                $"{instruction.Mnemonic} does not support {instruction.OpCount} OpCount or {instruction.OpCode.OperandSize} OperandSize!");
        }
    }

    private void TranslatePushfq(ref RegisterContext state)
    {
        state.Stack.PushExpression(state.RFlags);
    }

    private void TranslatePopfq(ref RegisterContext state)
    {
        state.RFlags = state.Stack.PopExpression();
    }

    private void TranslateSub(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var subExpr = (BitVecExpr)ctx.MkBVSub(e1, e2).Simplify();

        StoreExpr(subExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, subExpr);
        state.CalculateAuxFlag(ctx, e1, e2, subExpr);
        state.CalculateSubFlags(ctx, e1, e2, subExpr);
    }

    private void TranslateMul(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);
        var destExpr = instruction.Mnemonic switch
        {
            Mnemonic.Mul => (BitVecExpr)ctx.MkZeroExt(instruction.GetOperandSize(0) * 8, e1).Simplify(),
            Mnemonic.Imul => (BitVecExpr)ctx.MkSignExt(instruction.GetOperandSize(0) * 8, e1).Simplify(),
            _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
        };

        var src1Expr = instruction switch
        {
            { Mnemonic: Mnemonic.Mul } => instruction.GetOperandSize(0) switch
            {
                1 => (BitVecExpr)ctx.MkZeroExt(8, state.IcedRegisterToContextRegister(ctx, Register.AL).Item1)
                    .Simplify(),
                2 => (BitVecExpr)ctx.MkZeroExt(16, state.IcedRegisterToContextRegister(ctx, Register.AX).Item1)
                    .Simplify(),
                4 => (BitVecExpr)ctx.MkZeroExt(32, state.IcedRegisterToContextRegister(ctx, Register.EAX).Item1)
                    .Simplify(),
                8 => (BitVecExpr)ctx.MkZeroExt(64, state.IcedRegisterToContextRegister(ctx, Register.RAX).Item1)
                    .Simplify(),
                _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
            },
            { Mnemonic: Mnemonic.Imul } => instruction.OpCount switch
            {
                1 => instruction.GetOperandSize(0) switch
                {
                    1 => (BitVecExpr)ctx.MkSignExt(8, state.IcedRegisterToContextRegister(ctx, Register.AL).Item1)
                        .Simplify(),
                    2 => (BitVecExpr)ctx.MkSignExt(16, state.IcedRegisterToContextRegister(ctx, Register.AX).Item1)
                        .Simplify(),
                    4 => (BitVecExpr)ctx.MkSignExt(32, state.IcedRegisterToContextRegister(ctx, Register.EAX).Item1)
                        .Simplify(),
                    8 => (BitVecExpr)ctx.MkSignExt(64, state.IcedRegisterToContextRegister(ctx, Register.RAX).Item1)
                        .Simplify(),
                    _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
                },
                2 or 3 => (BitVecExpr)ctx.MkSignExt(instruction.GetOperandSize(1) * 8u,
                    ConcretizeOperand(instruction, 1, ref state).Item1).Simplify(),
                _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
            },
            _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
        };

        var src2Expr = instruction switch
        {
            { Mnemonic: Mnemonic.Imul, OpCount: 3 } => (BitVecExpr)ctx.MkSignExt(instruction.GetOperandSize(2) * 8u,
                ConcretizeOperand(instruction, 2, ref state).Item1).Simplify(),
            _ => null
        };

        var mulExpr = instruction switch
        {
            { Mnemonic: Mnemonic.Mul or Mnemonic.Imul, OpCount: 1 or 2 } => (BitVecExpr)ctx.MkBVMul(destExpr, src1Expr)
                .Simplify(),
            { Mnemonic: Mnemonic.Imul, OpCount: 3 } => (BitVecExpr)ctx.MkBVMul(src1Expr, src2Expr).Simplify(),
            _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
        };

        switch (instruction.OpCount)
        {
            case 1:
            {
                switch (mulExpr.SortSize)
                {
                    case 16:
                    {
                        state.StoreExprInContextRegister(ctx, mulExpr, Register.AX, ContextKind.RegisterL16);
                        break;
                    }

                    case 32:
                    {
                        state.StoreExprInContextRegister(ctx, ctx.MkExtract(31, 16, mulExpr).Simplify(), Register.DX,
                            ContextKind.RegisterL16);
                        state.StoreExprInContextRegister(ctx, ctx.MkExtract(15, 0, mulExpr).Simplify(), Register.AX,
                            ContextKind.RegisterL16);

                        break;
                    }

                    case 64:
                    {
                        state.StoreExprInContextRegister(ctx, ctx.MkExtract(63, 32, mulExpr).Simplify(), Register.EDX,
                            ContextKind.RegisterL32);
                        state.StoreExprInContextRegister(ctx, ctx.MkExtract(31, 0, mulExpr).Simplify(), Register.EAX,
                            ContextKind.RegisterL32);

                        break;
                    }

                    case 128:
                    {
                        state.StoreExprInContextRegister(ctx, ctx.MkExtract(127, 64, mulExpr).Simplify(), Register.RDX,
                            ContextKind.Register64);
                        state.StoreExprInContextRegister(ctx, ctx.MkExtract(63, 0, mulExpr).Simplify(), Register.RAX,
                            ContextKind.Register64);

                        break;
                    }

                    default:
                        throw new ArgumentOutOfRangeException($"{nameof(mulExpr)}", mulExpr.SortSize,
                            "TranslateMul can't handle mulExpr SortSize!");
                }


                break;
            }

            case 2:
            case 3:
            {
                destExpr = (BitVecExpr)ctx.MkExtract((uint)instruction.Op0Register.GetSize() * 8u - 1, 0u, mulExpr)
                    .Simplify();

                state.StoreExprInContextRegister(ctx, destExpr, instruction.Op0Register, destKind);
                break;
            }

            default:
                throw new ArgumentOutOfRangeException($"{nameof(instruction)}", instruction.OpCount,
                    "TranslateMul does not support this OpCount!");
        }

        switch (instruction.Mnemonic)
        {
            case Mnemonic.Mul:
            {
                var highExpr =
                    (BitVecExpr)ctx.MkExtract(mulExpr.SortSize - 1, mulExpr.SortSize / 2, mulExpr).Simplify();

                var flagExpr = (BoolExpr)ctx.MkDistinct(highExpr, ctx.MkBV(0, highExpr.SortSize)).Simplify();
                state.Of = flagExpr;
                state.Cf = flagExpr;

                break;
            }

            case Mnemonic.Imul:
            {
                switch (instruction.OpCount)
                {
                    case 1:
                    {
                        var halfMulSize = mulExpr.SortSize / 2;
                        var signExtLow = (BitVecExpr)ctx
                            .MkSignExt(halfMulSize, ctx.MkExtract(halfMulSize - 1, 0, mulExpr)).Simplify();

                        var flagExpr = (BoolExpr)ctx.MkDistinct(signExtLow, mulExpr).Simplify();
                        state.Of = flagExpr;
                        state.Cf = flagExpr;

                        break;
                    }

                    case 2:
                    case 3:
                    {
                        var flagExpr = (BoolExpr)ctx.MkDistinct(ctx.MkSignExt(destExpr.SortSize, destExpr), mulExpr)
                            .Simplify();
                        state.Of = flagExpr;
                        state.Cf = flagExpr;

                        break;
                    }

                    default:
                        throw new ArgumentOutOfRangeException($"{nameof(instruction)}", instruction.OpCount,
                            "TranslateMul does not support this OpCount!");
                }

                break;
            }

            default:
                throw new ArgumentOutOfRangeException($"{nameof(instruction)}", instruction.Mnemonic,
                    "TranslateMul does not support this Mnemonic!");
        }
    }

    private void TranslateDiv(Instruction instruction, ref RegisterContext state)
    {
        var e1 = ConcretizeOperand(instruction, 0, ref state).Item1;
        var src = instruction.Mnemonic switch
        {
            Mnemonic.Div => (BitVecExpr)ctx.MkZeroExt(instruction.GetOperandSize(0) * 8, e1).Simplify(),
            Mnemonic.Idiv => (BitVecExpr)ctx.MkSignExt(instruction.GetOperandSize(0) * 8, e1).Simplify(),
            _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
        };

        var e2 = instruction.GetOperandSize(0) switch
        {
            1 => state.IcedRegisterToContextRegister(ctx, Register.AX).Item1,
            2 => (BitVecExpr)ctx.MkConcat(state.IcedRegisterToContextRegister(ctx, Register.DX).Item1,
                state.IcedRegisterToContextRegister(ctx, Register.AX).Item1).Simplify(),
            4 => (BitVecExpr)ctx.MkConcat(state.IcedRegisterToContextRegister(ctx, Register.EDX).Item1,
                state.IcedRegisterToContextRegister(ctx, Register.EAX).Item1).Simplify(),
            8 => (BitVecExpr)ctx.MkConcat(state.IcedRegisterToContextRegister(ctx, Register.RDX).Item1,
                state.IcedRegisterToContextRegister(ctx, Register.RAX).Item1).Simplify(),
            _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
        };

        var divExpr = instruction.Mnemonic switch
        {
            Mnemonic.Div => (BitVecExpr)ctx.MkBVUDiv(e2, src).Simplify(),
            Mnemonic.Idiv => (BitVecExpr)ctx.MkBVSDiv(e2, src).Simplify(),
            _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
        };

        var remExpr = instruction.Mnemonic switch
        {
            Mnemonic.Div => (BitVecExpr)ctx.MkBVURem(e2, src).Simplify(),
            Mnemonic.Idiv => (BitVecExpr)ctx.MkBVSRem(e2, src).Simplify(),
            _ => throw new ArgumentOutOfRangeException($"{nameof(instruction)}")
        };

        switch (instruction.GetOperandSize(0))
        {
            case 1:
            {
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(7, 0, divExpr).Simplify(), Register.AL,
                    ContextKind.RegisterL8);
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(7, 0, remExpr).Simplify(), Register.AH,
                    ContextKind.RegisterH8);

                break;
            }

            case 2:
            {
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(15, 0, divExpr).Simplify(), Register.AX,
                    ContextKind.RegisterL16);
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(15, 0, remExpr).Simplify(), Register.DX,
                    ContextKind.RegisterL16);

                break;
            }

            case 4:
            {
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(31, 0, divExpr).Simplify(),
                    Register.EAX,
                    ContextKind.RegisterL32);
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(31, 0, remExpr).Simplify(),
                    Register.EDX,
                    ContextKind.RegisterL32);

                break;
            }

            case 8:
            {
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(63, 0, divExpr).Simplify(),
                    Register.RAX,
                    ContextKind.Register64);
                state.StoreExprInContextRegister(ctx, (BitVecExpr)ctx.MkExtract(63, 0, remExpr).Simplify(),
                    Register.RDX,
                    ContextKind.Register64);

                break;
            }
        }
    }

    private void TranslateAdd(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var addExpr = (BitVecExpr)ctx.MkBVAdd(e1, e2).Simplify();

        StoreExpr(addExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, addExpr);
        state.CalculateAuxFlag(ctx, e1, e2, addExpr);
        state.CalculateAddFlags(ctx, e1, e2, addExpr);
    }

    private void TranslateAdc(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        // if (Cf) { e2 + 1 } else { e2 + 0 }
        e2 = ctx.MkBVAdd(e2, (BitVecExpr)ctx.MkITE(state.Cf, ctx.MkBV(1, e2.SortSize), ctx.MkBV(0, e2.SortSize)));
        var addExpr = (BitVecExpr)ctx.MkBVAdd(e1, e2).Simplify();

        StoreExpr(addExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, addExpr);
        state.CalculateAuxFlag(ctx, e1, e2, addExpr);
        state.CalculateAddFlags(ctx, e1, e2, addExpr);
    }

    private void TranslateOr(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var orExpr = (BitVecExpr)ctx.MkBVOR(e1, e2).Simplify();

        StoreExpr(orExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, orExpr);
        state.Of = ctx.MkFalse();
        state.Cf = ctx.MkFalse();
    }

    private void TranslateXor(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var xorExpr = (BitVecExpr)ctx.MkBVXOR(e1, e2).Simplify();

        StoreExpr(xorExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, xorExpr);
        state.Of = ctx.MkFalse();
        state.Cf = ctx.MkFalse();
    }

    private void TranslateAnd(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var andExpr = (BitVecExpr)ctx.MkBVAND(e1, e2).Simplify();

        StoreExpr(andExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, andExpr);
        state.Of = ctx.MkFalse();
        state.Cf = ctx.MkFalse();
    }

    private void TranslateNeg(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var negExpr = (BitVecExpr)ctx.MkBVNeg(e1).Simplify();

        StoreExpr(negExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, negExpr);

        // Fake e2 to perform checks on af, cf, and of
        // e2 needs to have the opposite sign bit as e1
        var e2 = (BitVecExpr)ctx
            .MkConcat(ctx.MkBVNot(ctx.MkExtract(e1.SortSize - 1, e1.SortSize - 1, e1)), ctx.MkBV(0, e1.SortSize - 1))
            .Simplify();

        state.CalculateAuxFlag(ctx, e1, e2, negExpr);
        state.Of = (BoolExpr)ctx.MkEq(e1, ctx.MkBV(1, e1.SortSize)).Simplify();
        state.Cf = (BoolExpr)ctx.MkDistinct(e1, ctx.MkBV(0, e1.SortSize)).Simplify();
    }

    private void TranslateLea(Instruction instruction, ref RegisterContext state)
    {
        var (e2, _) = ConcretizeOperand(instruction, 1, ref state, true);

        // lea [reg], reg
        if (instruction.Op0Kind == OpKind.Memory)
        {
            throw new NotImplementedException($"{instruction.ToString()} is not supported by Lea handler!");
        }

        // lea reg, [reg]
        if (instruction.Op1Kind == OpKind.Memory)
        {
            var (_, destKind) = ConcretizeOperand(instruction, 0, ref state);
            state.StoreExprInContextRegister(ctx, e2, instruction.Op0Register, destKind);
        }
        else
        {
            throw new NotImplementedException($"{instruction.ToString()} is not supported by Lea handler!");
        }
    }

    private void TranslateNot(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var notExpr = (BitVecExpr)ctx.MkBVNot(e1).Simplify();

        StoreExpr(notExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
    }

    private void TranslateBt(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var bitOffset = destKind switch
        {
            ContextKind.RegisterL16 => (BitVecNum)ctx.MkBVSMod(e2, ctx.MkBV(16, e2.SortSize)).Simplify(),
            ContextKind.RegisterL32 => (BitVecNum)ctx.MkBVSMod(e2, ctx.MkBV(32, e2.SortSize)).Simplify(),
            ContextKind.Register64 => (BitVecNum)ctx.MkBVSMod(e2, ctx.MkBV(64, e2.SortSize)).Simplify(),
            ContextKind.Immediate => (BitVecNum)ctx.MkBVSMod(e2, ctx.MkBV(8, e2.SortSize)).Simplify(),
            _ => throw new NotImplementedException($"{instruction.ToString()} does not handle {destKind} ContextKind!")
        };

        if (bitOffset.UInt > 64)
            throw new InvalidOperationException($"bitOffset contains an impossible value: {bitOffset.BigInteger}!");

        switch (instruction.Mnemonic)
        {
            case Mnemonic.Bt:
            {
                state.Cf = (BoolExpr)ctx.MkEq(ctx.MkExtract(bitOffset.UInt, bitOffset.UInt, e1), ctx.MkBV(1, 1))
                    .Simplify();
                break;
            }

            case Mnemonic.Bts:
            {
                state.Cf = (BoolExpr)ctx.MkEq(ctx.MkExtract(bitOffset.UInt, bitOffset.UInt, e1), ctx.MkBV(1, 1))
                    .Simplify();

                var storeExpr = (BitVecExpr)ctx.MkBV(1, 1);

                if (bitOffset.UInt != e1.SortSize - 1)
                    storeExpr = (BitVecExpr)ctx
                        .MkConcat(ctx.MkExtract(e1.SortSize - 1, bitOffset.UInt + 1, e1), storeExpr).Simplify();

                if (bitOffset.UInt != 0)
                    storeExpr = (BitVecExpr)ctx
                        .MkConcat(storeExpr, ctx.MkExtract(bitOffset.UInt - 1, 0, e1)).Simplify();

                StoreExpr(storeExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

                break;
            }

            case Mnemonic.Btc:
            {
                var bitExpr = (BitVecExpr)ctx.MkExtract(bitOffset.UInt, bitOffset.UInt, e1).Simplify();
                state.Cf = (BoolExpr)ctx.MkEq(bitExpr, ctx.MkBV(1, 1)).Simplify();

                var storeExpr = ctx.MkBVNot(bitExpr);

                if (bitOffset.UInt != e1.SortSize - 1)
                    storeExpr = (BitVecExpr)ctx
                        .MkConcat(ctx.MkExtract(e1.SortSize - 1, bitOffset.UInt + 1, e1), storeExpr).Simplify();

                if (bitOffset.UInt != 0)
                    storeExpr = (BitVecExpr)ctx
                        .MkConcat(storeExpr, ctx.MkExtract(bitOffset.UInt - 1, 0, e1)).Simplify();

                StoreExpr(storeExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

                break;
            }

            case Mnemonic.Btr:
            {
                state.Cf = (BoolExpr)ctx.MkEq(ctx.MkExtract(bitOffset.UInt, bitOffset.UInt, e1), ctx.MkBV(1, 1))
                    .Simplify();

                var storeExpr = (BitVecExpr)ctx.MkBV(0, 1);

                if (bitOffset.UInt != e1.SortSize - 1)
                    storeExpr = (BitVecExpr)ctx
                        .MkConcat(ctx.MkExtract(e1.SortSize - 1, bitOffset.UInt + 1, e1), storeExpr).Simplify();

                if (bitOffset.UInt != 0)
                    storeExpr = (BitVecExpr)ctx
                        .MkConcat(storeExpr, ctx.MkExtract(bitOffset.UInt - 1, 0, e1)).Simplify();

                StoreExpr(storeExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

                break;
            }
        }
    }

    private void TranslateShift(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var memoryBits = instruction.MemorySize switch
        {
            MemorySize.Int64 or MemorySize.UInt64 => 5u,
            _ => 4u
        };

        var count = destKind switch
        {
            // Count is masked to only 5 bits
            ContextKind.Immediate or ContextKind.RegisterL8 or ContextKind.RegisterL16 or ContextKind.RegisterL32 =>
                (BitVecExpr)ctx.MkExtract(4, 0, e2).Simplify(),
            // except when rex.w prefix is set, in which case 6 bits
            ContextKind.Register64 => (BitVecExpr)ctx.MkExtract(5, 0, e2).Simplify(),
            ContextKind.Memory => (BitVecExpr)ctx.MkExtract(memoryBits, 0, e2).Simplify(),
            _ => throw new InvalidOperationException($"Impossible destKind in TranslateShift: {destKind}!")
        };

        count = (BitVecExpr)ctx.MkZeroExt(e1.SortSize - count.SortSize, count).Simplify();

        var shiftExpr = instruction switch
        {
            { Mnemonic: Mnemonic.Sal or Mnemonic.Shl or Mnemonic.Shlx } =>
                (BitVecExpr)ctx.MkBVSHL(e1, count).Simplify(),
            { Mnemonic: Mnemonic.Shr or Mnemonic.Shrx } => (BitVecExpr)ctx.MkBVLSHR(e1, count).Simplify(),
            { Mnemonic: Mnemonic.Sar or Mnemonic.Sarx } => (BitVecExpr)ctx.MkBVASHR(e1, count).Simplify(),
            _ => throw new InvalidOperationException($"Impossible Mnemonic in TranslateShift: {instruction.Mnemonic}!")
        };

        var prevShiftExpr = instruction switch
        {
            { Mnemonic: Mnemonic.Sal or Mnemonic.Shl or Mnemonic.Shlx } =>
                (BitVecExpr)ctx.MkBVSHL(e1, ctx.MkBVSub(count, ctx.MkBV(1, count.SortSize))).Simplify(),
            { Mnemonic: Mnemonic.Sar or Mnemonic.Sarx or Mnemonic.Shr or Mnemonic.Shrx } => (BitVecExpr)ctx
                .MkBVLSHR(e1, ctx.MkBVSub(count, ctx.MkBV(1, count.SortSize))).Simplify(),
            _ => throw new InvalidOperationException($"Impossible Mnemonic in TranslateShift: {instruction.Mnemonic}!")
        };

        state.Cf = instruction switch
        {
            { Mnemonic: Mnemonic.Sal or Mnemonic.Shl } => (BoolExpr)ctx
                .MkEq(ctx.MkExtract(prevShiftExpr.SortSize - 1, prevShiftExpr.SortSize - 1, prevShiftExpr),
                    ctx.MkBV(1, 1)).Simplify(),
            { Mnemonic: Mnemonic.Sar or Mnemonic.Shr } => (BoolExpr)ctx
                .MkEq(ctx.MkExtract(0, 0, prevShiftExpr), ctx.MkBV(1, 1))
                .Simplify(),
            _ => state.Cf
        };

        state.Of = (BoolExpr)ctx.MkITE(ctx.MkEq(count, ctx.MkBV(1, count.SortSize)),
            instruction switch
            {
                { Mnemonic: Mnemonic.Sal or Mnemonic.Shl } => ctx.MkXor(
                        ctx.MkEq(ctx.MkExtract(shiftExpr.SortSize - 1, shiftExpr.SortSize - 1, shiftExpr),
                            ctx.MkBV(1, 1)), state.Cf)
                    .Simplify(),
                { Mnemonic: Mnemonic.Sar } => ctx.MkFalse(),
                { Mnemonic: Mnemonic.Shr } => ctx.MkEq(
                    ctx.MkExtract(e1.SortSize - 1, e1.SortSize - 1, e1), ctx.MkBV(1, 1)),
                _ => state.Of
            }, state.Of).Simplify();

        StoreExpr(shiftExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
        state.CalculateCommonFlags(ctx, shiftExpr);
        state.Af = ctx.MkFalse();
    }

    private void TranslateShrd(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);
        var (e3, _) = ConcretizeOperand(instruction, 2, ref state);

        var countExpr = (BitVecExpr)ctx.MkZeroExt(e1.SortSize - e3.SortSize, ctx.MkBVURem(e3, ctx.MkBV(e1.SortSize != 64 ? 32 : 64, e3.SortSize))).Simplify();
        var shiftedDest = (BitVecExpr)ctx.MkBVLSHR(e1, countExpr).Simplify();

        var compCountExpr = (BitVecExpr)ctx.MkBVSub(ctx.MkBV(e1.SortSize, countExpr.SortSize), countExpr).Simplify();
        var shiftedSource = (BitVecExpr)ctx.MkBVSHL(e2, compCountExpr).Simplify();

        var resExpr = (BitVecExpr)ctx.MkBVOR(shiftedDest, shiftedSource).Simplify();

        StoreExpr(resExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        var cfBitPos = (BitVecExpr)ctx.MkBVSub(countExpr, ctx.MkBV(1, countExpr.SortSize)).Simplify();
        var cfExpr = (BitVecExpr)ctx.MkBVLSHR(e1, cfBitPos).Simplify();
        cfExpr = (BitVecExpr)ctx.MkBVAND(cfExpr, ctx.MkBV(1, cfExpr.SortSize)).Simplify();

        state.CalculateCommonFlags(ctx, resExpr);
        state.Cf = (BoolExpr)ctx.MkEq(ctx.MkExtract(cfExpr.SortSize - 1, cfExpr.SortSize - 1, cfExpr), ctx.MkBV(1, 1)).Simplify();
        state.Of = (BoolExpr)ctx.MkITE(ctx.MkEq(countExpr, ctx.MkBV(1, countExpr.SortSize)),
            ctx.MkDistinct(ctx.MkExtract(e1.SortSize - 1, e1.SortSize - 1, e1),
                ctx.MkExtract(resExpr.SortSize - 1, resExpr.SortSize - 1, resExpr)),
            state.Of);
    }

    private void TranslateRotate(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var memoryBits = instruction.MemorySize switch
        {
            MemorySize.Int64 or MemorySize.UInt64 => 5u,
            _ => 4u
        };

        var count = destKind switch
        {
            // Count is masked to only 5 bits
            ContextKind.Immediate or ContextKind.RegisterL8 or ContextKind.RegisterL16 or ContextKind.RegisterL32 =>
                (BitVecExpr)ctx.MkExtract(4, 0, e2).Simplify(),
            // except when rex.w is 64 bits, in which case 6 bits
            ContextKind.Register64 => (BitVecExpr)ctx.MkExtract(5, 0, e2).Simplify(),
            ContextKind.Memory => (BitVecExpr)ctx.MkExtract(memoryBits, 0, e2).Simplify(),
            _ => throw new InvalidOperationException($"Impossible destKind in TranslateRotate: {destKind}!")
        };

        BitVecExpr resultExpr;

        switch (instruction.Mnemonic)
        {
            case Mnemonic.Rol:
                resultExpr = (BitVecExpr)ctx.MkBVRotateLeft(e1, ctx.MkZeroExt(e1.SortSize - count.SortSize, count))
                    .Simplify();

                // if (count != 0) { LSB(DEST) } else { state.Cf }
                state.Cf = (BoolExpr)ctx.MkITE(ctx.MkDistinct(count, ctx.MkBV(0, count.SortSize)), (BoolExpr)ctx.MkEq(
                    ctx.MkExtract(0, 0, resultExpr),
                    ctx.MkBV(1, 1)).Simplify(), state.Cf);

                // if (count == 1) { MSB(DEST) XOR Cf } else { state.Of }
                state.Of = (BoolExpr)ctx.MkITE(ctx.MkEq(count, ctx.MkBV(1, count.SortSize)),
                    ctx.MkXor(ctx.MkEq(ctx.MkExtract(e1.SortSize - 1, e1.SortSize - 1, resultExpr), ctx.MkBV(1, 1)),
                        state.Cf), state.Of).Simplify();

                break;

            case Mnemonic.Ror:
                resultExpr = (BitVecExpr)ctx.MkBVRotateRight(e1, ctx.MkZeroExt(e1.SortSize - count.SortSize, count))
                    .Simplify();

                state.Cf = (BoolExpr)ctx.MkITE(ctx.MkDistinct(count, ctx.MkBV(0, count.SortSize)),
                        ctx.MkEq(ctx.MkExtract(resultExpr.SortSize - 1, resultExpr.SortSize - 1, resultExpr),
                            ctx.MkBV(1, 1)), state.Cf)
                    .Simplify(); // MSB of dest

                state.Of = (BoolExpr)ctx.MkITE(ctx.MkEq(count, ctx.MkBV(1, count.SortSize)),
                    ctx.MkXor(
                        ctx.MkEq(ctx.MkExtract(e1.SortSize - 1, e1.SortSize - 1, resultExpr), ctx.MkBV(1, 1)),
                        ctx.MkEq(ctx.MkExtract(e1.SortSize - 2, e1.SortSize - 2, resultExpr), ctx.MkBV(1, 1))),
                    state.Of).Simplify();

                break;

            default:
                throw new ArgumentOutOfRangeException(nameof(instruction.Mnemonic), instruction.Mnemonic, null);
            // Add cases for RCL and RCR as needed, considering the carry flag and specific rotation logic
        }

        // Store the result in the newState
        StoreExpr(resultExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
    }

    private void TranslateInc(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var e2 = ctx.MkBV(1, e1.SortSize);
        var incExpr = ctx.MkBVAdd(e1, e2);

        StoreExpr(incExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        var oldCf = state.Cf;
        state.CalculateCommonFlags(ctx, incExpr);
        state.CalculateAuxFlag(ctx, e1, e2, incExpr);
        state.CalculateAddFlags(ctx, e1, e2, incExpr);
        state.Cf = oldCf;
    }

    private void TranslateDec(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var e2 = ctx.MkBV(1, e1.SortSize);
        var decExpr = ctx.MkBVSub(e1, e2);

        StoreExpr(decExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        var oldCf = state.Cf;
        state.CalculateCommonFlags(ctx, decExpr);
        state.CalculateAuxFlag(ctx, e1, e2, decExpr);
        state.CalculateSubFlags(ctx, e1, e2, decExpr);
        state.Cf = oldCf;
    }

    private void TranslateConvertExtend(Instruction instruction, ref RegisterContext state)
    {
        var (e1, _) = instruction switch
        {
            { Mnemonic: Mnemonic.Cbw } => state.IcedRegisterToContextRegister(ctx, Register.AL),
            { Mnemonic: Mnemonic.Cwd or Mnemonic.Cwde } => state.IcedRegisterToContextRegister(ctx, Register.AX),
            { Mnemonic: Mnemonic.Cdq or Mnemonic.Cdqe } => state.IcedRegisterToContextRegister(ctx, Register.EAX),
            { Mnemonic: Mnemonic.Cqo } => state.IcedRegisterToContextRegister(ctx, Register.RAX),
            _ => (default, default)
        };

        if (e1 == null)
            throw new InvalidOperationException($"Concretized expression is null on {instruction.ToString()}!");

        var storeExpr = instruction switch
        {
            { Mnemonic: Mnemonic.Cbw or Mnemonic.Cwde or Mnemonic.Cdqe } => (BitVecExpr)ctx
                .MkSignExt(e1.SortSize, e1).Simplify(),
            { Mnemonic: Mnemonic.Cwd or Mnemonic.Cdq or Mnemonic.Cqo } => (BitVecExpr)ctx.MkSignExt(e1.SortSize - 1,
                ctx.MkExtract(e1.SortSize - 1, e1.SortSize - 1, e1)).Simplify(),
            _ => default
        };

        if (storeExpr == null)
            throw new InvalidOperationException($"Concretzed store expression is null on {instruction.ToString()}");

        switch (instruction.Mnemonic)
        {
            case Mnemonic.Cwd:
            {
                state.StoreExprInContextRegister(ctx, storeExpr, Register.DX, ContextKind.RegisterL16);
                break;
            }

            case Mnemonic.Cdq:
            {
                state.StoreExprInContextRegister(ctx, storeExpr, Register.EDX, ContextKind.RegisterL32);
                break;
            }

            case Mnemonic.Cqo:
            {
                state.StoreExprInContextRegister(ctx, storeExpr, Register.RDX, ContextKind.Register64);
                break;
            }

            case Mnemonic.Cbw:
            {
                state.StoreExprInContextRegister(ctx, storeExpr, Register.AX, ContextKind.RegisterL16);
                break;
            }

            case Mnemonic.Cwde:
            {
                state.StoreExprInContextRegister(ctx, storeExpr, Register.EAX, ContextKind.RegisterL32);
                break;
            }

            case Mnemonic.Cdqe:
            {
                state.StoreExprInContextRegister(ctx, storeExpr, Register.RAX, ContextKind.Register64);
                break;
            }

            default:
            {
                throw new NotImplementedException($"{instruction.Mnemonic} is not handled!");
            }
        }
    }

    private void TranslateBswap(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr storeExpr;
        switch (destKind)
        {
            case ContextKind.Register64:
            {
                var byte7 = ctx.MkExtract(63, 56, e1);
                var byte6 = ctx.MkExtract(55, 48, e1);
                var byte5 = ctx.MkExtract(47, 40, e1);
                var byte4 = ctx.MkExtract(39, 32, e1);
                var byte3 = ctx.MkExtract(31, 24, e1);
                var byte2 = ctx.MkExtract(23, 16, e1);
                var byte1 = ctx.MkExtract(15, 8, e1);
                var byte0 = ctx.MkExtract(7, 0, e1);

                storeExpr = ctx.MkConcat(byte0,
                    ctx.MkConcat(byte1,
                        ctx.MkConcat(byte2,
                            ctx.MkConcat(byte3,
                                ctx.MkConcat(byte4, ctx.MkConcat(byte5, ctx.MkConcat(byte6, byte7)))))));

                break;
            }

            case ContextKind.RegisterL32:
            {
                var byte3 = ctx.MkExtract(31, 24, e1);
                var byte2 = ctx.MkExtract(23, 16, e1);
                var byte1 = ctx.MkExtract(15, 8, e1);
                var byte0 = ctx.MkExtract(7, 0, e1);

                storeExpr = ctx.MkConcat(byte0, ctx.MkConcat(byte1, ctx.MkConcat(byte2, byte3)));

                break;
            }

            default:
            {
                throw new ArgumentOutOfRangeException(nameof(destKind), $"{destKind}", null);
            }
        }

        state.StoreExprInContextRegister(ctx, storeExpr, instruction.Op0Register, destKind);
    }

    // xadd dest, src
    private void TranslateXadd(Instruction instruction, ref RegisterContext state)
    {
        var (e1, e1Kind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (e1Kind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, e2Kind) = ConcretizeOperand(instruction, 1, ref state);

        var addExpr = (BitVecExpr)ctx.MkBVAdd(e1, e2).Simplify();

        StoreExpr(addExpr, e1Kind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
        state.StoreExprInContextRegister(ctx, e1, instruction.Op1Register, e2Kind);

        state.CalculateCommonFlags(ctx, addExpr);
        state.CalculateAuxFlag(ctx, e1, e2, addExpr);
        state.Cf = (BoolExpr)ctx.MkOr(ctx.MkBVULT(addExpr, e1), ctx.MkBVULT(addExpr, e2)).Simplify();

        var resSign = ctx.MkBVSLT(addExpr, ctx.MkBV(0, addExpr.SortSize));
        var destSign = ctx.MkBVSLT(e1, ctx.MkBV(0, e1.SortSize));
        var srcSign = ctx.MkBVSLT(e2, ctx.MkBV(0, e2.SortSize));
        state.Of = (BoolExpr)ctx.MkAnd(ctx.MkEq(destSign, srcSign), ctx.MkDistinct(destSign, resSign)).Simplify();
    }

    private void TranslateSbb(Instruction instruction, ref RegisterContext state)
    {
        var (e1, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        // if (Cf) { src + 1 } else { src + 0 }
        var oldCf = (BitVecExpr)ctx.MkITE(state.Cf, ctx.MkBV(1, e2.SortSize), ctx.MkBV(0, e2.SortSize));
        e2 = (BitVecExpr)ctx.MkBVAdd(e2, oldCf).Simplify();

        var subExpr = (BitVecExpr)ctx.MkBVSub(e1, e2).Simplify();

        StoreExpr(subExpr, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);

        state.CalculateCommonFlags(ctx, subExpr);
        state.CalculateAuxFlag(ctx, e1, e2, subExpr);
        state.Cf = (BoolExpr)ctx.MkBVULT(e1, e2).Simplify();
        state.Of = (BoolExpr)ctx.MkBVSLT(ctx.MkBVAND(ctx.MkBVXOR(e1, e2), ctx.MkBVXOR(e1, ctx.MkBVSub(subExpr, oldCf))),
            ctx.MkBV(0, subExpr.SortSize)).Simplify();
    }

    private void TranslateXchg(Instruction instruction, ref RegisterContext state)
    {
        var (e1, e1Kind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (e1Kind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var (e2, e2Kind) = ConcretizeOperand(instruction, 1, ref state);

        BitVecExpr? e2Addr = null;
        if (e2Kind == ContextKind.Memory)
            e2Addr = ConcretizeOperand(instruction, 1, ref state, true).Item1;

        StoreExpr(e1, e2Kind, ref state, instruction.Op1Register, e2Addr, instruction.MemorySize);
        StoreExpr(e2, e1Kind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
    }

    private void TranslateSet(Instruction instruction, ref RegisterContext state)
    {
        var (_, destKind) = ConcretizeOperand(instruction, 0, ref state);

        BitVecExpr? e1Addr = null;
        if (destKind == ContextKind.Memory)
            e1Addr = ConcretizeOperand(instruction, 0, ref state, true).Item1;

        var conExpr = instruction.Mnemonic switch
        {
            Mnemonic.Seta => ctx.MkAnd(ctx.MkNot(state.Cf), ctx.MkNot(state.Zf)),
            Mnemonic.Setae => ctx.MkNot(state.Cf),
            Mnemonic.Setb => state.Cf,
            Mnemonic.Setbe => ctx.MkOr(state.Cf, state.Zf),
            Mnemonic.Sete => state.Zf,
            Mnemonic.Setg => ctx.MkAnd(ctx.MkNot(state.Zf), ctx.MkEq(state.Sf, state.Of)),
            Mnemonic.Setge => ctx.MkEq(state.Sf, state.Of),
            Mnemonic.Setl => ctx.MkXor(state.Sf, state.Of),
            Mnemonic.Setle => ctx.MkOr(state.Zf, ctx.MkXor(state.Sf, state.Of)),
            Mnemonic.Setne => ctx.MkNot(state.Zf),
            Mnemonic.Setno => ctx.MkNot(state.Of),
            Mnemonic.Setnp => ctx.MkNot(state.Pf),
            Mnemonic.Setns => ctx.MkNot(state.Sf),
            Mnemonic.Seto => state.Of,
            Mnemonic.Setp => state.Pf,
            Mnemonic.Sets => state.Sf,
            _ => throw new ArgumentOutOfRangeException(nameof(instruction.Mnemonic), $"{instruction.Mnemonic}", null)
        };

        var e2 = (BitVecExpr)ctx.MkITE(conExpr, ctx.MkBV(1, 8), ctx.MkBV(0, 8)).Simplify();

        StoreExpr(e2, destKind, ref state, instruction.Op0Register, e1Addr, instruction.MemorySize);
    }

    private void TranslateCmp(Instruction instruction, ref RegisterContext state)
    {
        var (e1, _) = ConcretizeOperand(instruction, 0, ref state);
        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        if (e1.SortSize > e2.SortSize)
            e2 = (BitVecExpr)ctx.MkSignExt(e1.SortSize - e2.SortSize, e2).Simplify();

        var subExpr = (BitVecExpr)ctx.MkBVSub(e1, e2).Simplify();

        state.CalculateCommonFlags(ctx, subExpr);
        state.CalculateAuxFlag(ctx, e1, e2, subExpr);
        state.CalculateSubFlags(ctx, e1, e2, subExpr);
    }

    private void TranslateCall(Instruction instruction, ref RegisterContext state)
    {
        state.Stack.PushExpression(instruction.NextIP == 0
            ? ctx.MkBVConst($"ret_addr_{instruction.NearBranchTarget:X}", 64)
            : ctx.MkBV(instruction.NextIP, 64));
    }

    private void TranslateRet(Instruction instruction, ref RegisterContext state)
    {
        state.Stack.PopExpression();

        state.Stack.StackPointer =
            (BitVecNum)ctx.MkBVAdd(state.Stack.StackPointer, ctx.MkBV(instruction.GetImmediate(0), 64)).Simplify();
    }

    private void TranslateRdtsc(Instruction _, ref RegisterContext state)
    {
        state.StoreExprInContextRegister(ctx, ctx.MkBVConst("upper_tsc", 32), Register.EDX, ContextKind.RegisterL32);
        state.StoreExprInContextRegister(ctx, ctx.MkBVConst("lower_tsc", 32), Register.EAX, ContextKind.RegisterL32);
    }

    private void TranslateTest(Instruction instruction, ref RegisterContext state)
    {
        var (e1, _) = ConcretizeOperand(instruction, 0, ref state);
        var (e2, _) = ConcretizeOperand(instruction, 1, ref state);

        var andExpr = (BitVecExpr)ctx.MkBVAND(e1, e2).Simplify();

        state.CalculateCommonFlags(ctx, andExpr);
        state.Cf = ctx.MkFalse();
        state.Of = ctx.MkFalse();
    }

    public void TranslateInstruction(Instruction instruction, ref RegisterContext state)
    {
        state.Rip = (BitVecExpr)ctx.MkBVAdd(state.Rip, ctx.MkBV((ulong)instruction.Length, 64)).Simplify();

        switch (instruction.Mnemonic)
        {
            case Mnemonic.Push:
            {
                TranslatePush(instruction, ref state);
                break;
            }

            case Mnemonic.Pop:
            {
                TranslatePop(instruction, ref state);
                break;
            }

            case Mnemonic.Pushfq:
            {
                TranslatePushfq(ref state);
                break;
            }

            case Mnemonic.Popfq:
            {
                TranslatePopfq(ref state);
                break;
            }

            case Mnemonic.Mov:
            case Mnemonic.Movsx:
            case Mnemonic.Movsxd:
            case Mnemonic.Movzx:
            {
                TranslateMov(instruction, ref state);
                break;
            }

            case Mnemonic.Cmova:
            case Mnemonic.Cmovae:
            case Mnemonic.Cmovb:
            case Mnemonic.Cmovbe:
            case Mnemonic.Cmove:
            case Mnemonic.Cmovg:
            case Mnemonic.Cmovge:
            case Mnemonic.Cmovl:
            case Mnemonic.Cmovle:
            case Mnemonic.Cmovne:
            case Mnemonic.Cmovno:
            case Mnemonic.Cmovnp:
            case Mnemonic.Cmovns:
            case Mnemonic.Cmovo:
            case Mnemonic.Cmovp:
            case Mnemonic.Cmovs:
            {
                TranslateCmov(instruction, ref state);
                break;
            }

            case Mnemonic.Lea:
            {
                TranslateLea(instruction, ref state);
                break;
            }

            case Mnemonic.Add:
            {
                TranslateAdd(instruction, ref state);
                break;
            }

            case Mnemonic.Adc:
            {
                TranslateAdc(instruction, ref state);
                break;
            }

            case Mnemonic.Sub:
            {
                TranslateSub(instruction, ref state);
                break;
            }

            case Mnemonic.Mul:
            case Mnemonic.Imul:
            {
                TranslateMul(instruction, ref state);
                break;
            }

            case Mnemonic.Div:
            case Mnemonic.Idiv:
            {
                TranslateDiv(instruction, ref state);
                break;
            }

            case Mnemonic.Neg:
            {
                TranslateNeg(instruction, ref state);
                break;
            }

            case Mnemonic.Or:
            {
                TranslateOr(instruction, ref state);
                break;
            }

            case Mnemonic.Xor:
            {
                TranslateXor(instruction, ref state);
                break;
            }

            case Mnemonic.And:
            {
                TranslateAnd(instruction, ref state);
                break;
            }

            case Mnemonic.Not:
            {
                TranslateNot(instruction, ref state);
                break;
            }

            case Mnemonic.Bt:
            case Mnemonic.Btc:
            case Mnemonic.Btr:
            case Mnemonic.Bts:
            {
                TranslateBt(instruction, ref state);
                break;
            }

            case Mnemonic.Sal:
            case Mnemonic.Sar:
            case Mnemonic.Sarx:
            case Mnemonic.Shl:
            case Mnemonic.Shlx:
            case Mnemonic.Shr:
            case Mnemonic.Shrx:
            {
                TranslateShift(instruction, ref state);
                break;
            }

            case Mnemonic.Shrd:
            {
                TranslateShrd(instruction, ref state);
                break;
            }

            case Mnemonic.Rcl:
            case Mnemonic.Rcr:
            case Mnemonic.Rol:
            case Mnemonic.Ror:
            {
                TranslateRotate(instruction, ref state);
                break;
            }

            case Mnemonic.Inc:
            {
                TranslateInc(instruction, ref state);
                break;
            }

            case Mnemonic.Dec:
            {
                TranslateDec(instruction, ref state);
                break;
            }

            case Mnemonic.Cwd:
            case Mnemonic.Cdq:
            case Mnemonic.Cqo:
            case Mnemonic.Cbw:
            case Mnemonic.Cwde:
            case Mnemonic.Cdqe:
            {
                TranslateConvertExtend(instruction, ref state);
                break;
            }

            case Mnemonic.Bswap:
            {
                TranslateBswap(instruction, ref state);
                break;
            }

            case Mnemonic.Xadd:
            {
                TranslateXadd(instruction, ref state);
                break;
            }

            case Mnemonic.Sbb:
            {
                TranslateSbb(instruction, ref state);
                break;
            }

            case Mnemonic.Xchg:
            {
                TranslateXchg(instruction, ref state);
                break;
            }

            case Mnemonic.Seta:
            case Mnemonic.Setae:
            case Mnemonic.Setb:
            case Mnemonic.Setbe:
            case Mnemonic.Sete:
            case Mnemonic.Setg:
            case Mnemonic.Setge:
            case Mnemonic.Setl:
            case Mnemonic.Setle:
            case Mnemonic.Setne:
            case Mnemonic.Setno:
            case Mnemonic.Setnp:
            case Mnemonic.Setns:
            case Mnemonic.Seto:
            case Mnemonic.Setp:
            case Mnemonic.Sets:
            {
                TranslateSet(instruction, ref state);
                break;
            }

            case Mnemonic.Cmp:
            {
                TranslateCmp(instruction, ref state);
                break;
            }

            case Mnemonic.Call:
            {
                TranslateCall(instruction, ref state);
                break;
            }

            case Mnemonic.Ret:
            {
                TranslateRet(instruction, ref state);
                break;
            }

            case Mnemonic.Rdtsc:
            {
                TranslateRdtsc(instruction, ref state);
                break;
            }

            case Mnemonic.Test:
            {
                TranslateTest(instruction, ref state);
                break;
            }

            default:
                throw new NotImplementedException($"{instruction.Mnemonic} is not handled!");
        }

        state.Simplify();
    }
}