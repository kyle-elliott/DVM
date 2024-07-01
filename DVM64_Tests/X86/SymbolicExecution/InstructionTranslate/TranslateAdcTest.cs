using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateAdcTest
{
    [Fact]
    public void TranslateAdc_BasicAdc()
    {
        var value = 0xB1;
        var inst = Instruction.Create(Code.Adc_rm8_imm8, Register.BL, value);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rbx = ctx.MkBV(1, 64),
            Cf = ctx.MkFalse()
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
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0xB2, rbxExpr.BigInteger);
    }

    [Fact]
    public void TranslateAdc_OverflowNoCF()
    {
        var value = 0x01;
        var inst = Instruction.Create(Code.Adc_rm8_imm8, Register.BL, value);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rbx = ctx.MkBV(0xFF, 64),
            Cf = ctx.MkFalse()
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
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x00, rbxExpr.BigInteger);
    }

    [Fact]
    public void TranslateAdc_OverflowWithCF()
    {
        var value = 0x01;
        var inst = Instruction.Create(Code.Adc_rm8_imm8, Register.BL, value);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rbx = ctx.MkBV(0xFE, 64),
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
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x00, rbxExpr.BigInteger);
    }
}