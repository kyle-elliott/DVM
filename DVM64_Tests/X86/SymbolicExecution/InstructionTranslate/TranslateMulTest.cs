using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateMulTest
{
    [Fact]
    public void TranslateImul_rm16()
    {
        var inst = Instruction.Create(Code.Imul_rm16, Register.AX);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x00000000072AA62C, 64),
            Rdx = ctx.MkBV(0x000000000000001B, 64),
            Of = ctx.MkFalse(),
            Cf = ctx.MkTrue()
        };
        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var cfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Cf, true);
        var ofExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Of, true);
        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, ofExpr.BoolValue);
        Assert.Equal(0x00000000072A1790, raxExpr.BigInteger);
        Assert.Equal(0x0000000000001F85, rdxExpr.BigInteger);
    }

    [Fact]
    public void TranslateMul_rm32_1()
    {
        var inst = Instruction.Create(Code.Mul_rm32, Register.EBX);

        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-8, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x00000000FFFFFFDD, 64),
            Rbx = ctx.MkBV(0x000000000000FFFF, 64),
            Rdx = ctx.MkBV(0x00000000FFFFFFFF, 64),
            Of = ctx.MkTrue(),
            Cf = ctx.MkTrue()
        };
        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var cfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Cf, true);
        var ofExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Of, true);
        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, ofExpr.BoolValue);
        Assert.Equal(0x00000000FFDD0023, raxExpr.BigInteger);
        Assert.Equal(0x000000000000FFFE, rdxExpr.BigInteger);
    }

    [Fact]
    public void TranslateMul_rm32()
    {
        var inst = Instruction.Create(Code.Mul_rm32, Register.R10D);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0xFFFFFFFF1918463B, 64),
            Rdx = ctx.MkBV(0x000000022F2CBFFE, 64),
            R10 = ctx.MkBV(0x000000000000004D, 64),
            Of = ctx.MkTrue(),
            Cf = ctx.MkFalse()
        };
        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var cfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Cf, true);
        var ofExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Of, true);
        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, ofExpr.BoolValue);
        Assert.Equal(0x000000008C4D1FBF, raxExpr.BigInteger);
        Assert.Equal(0x0000000000000007, rdxExpr.BigInteger);
    }

    [Fact]
    public void TranslateImul_r64_rm64()
    {
        var inst = Instruction.Create(Code.Imul_r64_rm64, Register.RCX, Register.RBX);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rbx = ctx.MkBV(0x0000000809636781, 64),
            Rcx = ctx.MkBV(0x0000000000FFFFFF, 64),
            Of = ctx.MkTrue(),
            Cf = ctx.MkTrue()
        };
        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var cfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Cf, true);
        var pfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Pf, true);
        var afExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Af, true);
        var zfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Zf, true);
        var sfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Sf, true);
        var ofExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Of, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x0809635F779C987F, rcxExpr.BigInteger);
    }
}