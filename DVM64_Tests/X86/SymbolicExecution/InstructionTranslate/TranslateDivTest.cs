using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateDivTest
{
    [Fact]
    public void TranslateIdiv_rm32()
    {
        var inst = Instruction.Create(Code.Idiv_rm32, Register.EBX);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0xC701A3501EE64C00, 64),
            Rbx = ctx.MkBV(0x0000000090580000, 64),
            Rdx = ctx.MkBV(0x00000000000260DB, 64)
        };

        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);

        Assert.Equal(0x00000000FFFA8C0C, raxExpr.BigInteger);
        Assert.Equal(0x0000000090580000, rbxExpr.BigInteger);
        Assert.Equal(0x000000003AC64C00, rdxExpr.BigInteger);
    }

    [Fact]
    public void TranslateIdiv_rm64()
    {
        var inst = Instruction.Create(Code.Idiv_rm64, Register.RCX);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x00000000FFDE0021, 64),
            Rcx = ctx.MkBV(0x00000000FFFF5F3E, 64),
            Rdx = ctx.MkBV(0x000000000000FFFE, 64)
        };

        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);

        Assert.Equal(0x0000FFFEA0C1236F, raxExpr.BigInteger);
        Assert.Equal(0x00000000FFFF5F3E, rcxExpr.BigInteger);
        Assert.Equal(0x0000000088603A3F, rdxExpr.BigInteger);
    }
}