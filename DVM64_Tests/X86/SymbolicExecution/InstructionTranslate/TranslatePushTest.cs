using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslatePushTest
{
    [Fact]
    public void TranslatePushReg_64()
    {
        var inst = Instruction.Create(Code.Push_r64, Register.RBP);
        var ctx = new Context();
        var stackStart = 0x1000;
        var rbpValue = 0x1337;

        var initialContext = new RegisterContext(ctx, ctx.MkBV(stackStart, 64), ctx.MkBV(0, 64))
        {
            Rbp = ctx.MkBV(rbpValue, 64)
        };
        
        var reader = new ShellCodeReader(64, 0, [
            inst
        ]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);
        var rbpExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbp, true);

        Assert.Equal(stackStart - inst.Op0Register.GetSize(), rspExpr.BigInteger);
        Assert.Equal(rbpValue, rbpExpr.BigInteger);
        Assert.Contains(rbpExpr, initialContext.Stack);
    }
    
    [Fact]
    public void TranslatePushReg_32()
    {
        var inst = Instruction.Create(Code.Push_r32, Register.EBP);
        var ctx = new Context();
        var stackStart = 0x1000;
        var rbpValue = 0x1337;

        var initialContext = new RegisterContext(ctx, ctx.MkBV(stackStart, 32), ctx.MkBV(0, 64))
        {
            Rbp = ctx.MkBV(rbpValue, 32)
        };
        
        var reader = new ShellCodeReader(32, 0, [
            inst
        ]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        Assert.Throws<NotImplementedException>(() => { translate.TranslateInstruction(inst, ref initialContext); }); 
        return;
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer!, true);
        var rbpExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbp!, true);

        Assert.Equal(stackStart - inst.Op0Register.GetSize(), rspExpr.BigInteger);
        Assert.Equal(rbpValue, rbpExpr.BigInteger);
        Assert.True(initialContext.Stack.Contains(rbpExpr));
    }
    
    [Fact]
    public void TranslatePushReg_16()
    {
        var inst = Instruction.Create(Code.Push_r16, Register.BP);
        var ctx = new Context();
        var stackStart = 0x1000;
        var rbpValue = 0x1337;

        var initialContext = new RegisterContext(ctx, ctx.MkBV(stackStart, 16), ctx.MkBV(0, 64))
        {
            Rbp = ctx.MkBV(rbpValue, 16)
        };
        
        var reader = new ShellCodeReader(16, 0, [
            inst
        ]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        Assert.Throws<NotImplementedException>(() => { translate.TranslateInstruction(inst, ref initialContext); }); 
        return;
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);
        var rbpExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbp, true);

        Assert.Equal(stackStart - inst.Op0Register.GetSize(), rspExpr.BigInteger);
        Assert.Equal(rbpValue, rbpExpr.BigInteger);
        Assert.True(initialContext.Stack.Contains(rbpExpr));
    }
    
    [Fact]
    public void TranslatePushImm_32()
    {
        var value = 0x1337;
        var inst = Instruction.Create(Code.Pushd_imm32, value);
        var ctx = new Context();
        var stackStart = 0x1000;
        
        var initialContext = new RegisterContext(ctx, ctx.MkBV(stackStart, 32), ctx.MkBV(0, 64));
        
        var reader = new ShellCodeReader(32, 0, [
            inst
        ]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        Assert.Throws<NotImplementedException>(() => { translate.TranslateInstruction(inst, ref initialContext); }); 
        return;
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);

        Assert.Equal(stackStart - inst.Op0Register.GetSize(), rspExpr.BigInteger);
        Assert.Equal(value, ((BitVecNum)initialContext.Stack.Last()).BigInteger);
    }
}