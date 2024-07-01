using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateShiftTest
{
    [Fact]
    public void TranslateSar_rm16_imm8()
    {
        var inst = Instruction.Create(Code.Sar_rm16_imm8, Register.DX, 0x81);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rdx = ctx.MkBV(0x000000000000468F, 64)
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
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x0000000000002347, rdxExpr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64)
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateShl_rm32_cl()
    {
        var inst = Instruction.Create(Code.Shl_rm32_CL, Register.ECX, Register.CL);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rcx = ctx.MkBV(0x00000000AF38761B, 64),
            Pf = ctx.MkTrue(),
            Sf = ctx.MkTrue()
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
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x00000000D8000000, rcxExpr.BigInteger);
    }

    [Fact]
    public void TranslateShl_rm64_cl()
    {
        var inst = Instruction.Create(Code.Shl_rm64_CL, Register.RDX, Register.CL);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rcx = ctx.MkBV(0x00000000FFFF5F3E, 64),
            Rdx = ctx.MkBV(0x0000000088603A3F, 64)
        };

        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);

        Assert.Equal(0x00000000FFFF5F3E, rcxExpr.BigInteger);
        Assert.Equal(0xC000000000000000, rdxExpr.BigInteger);
    }

    [Fact]
    public void TranslateSar_rm8_imm8()
    {
        var inst = Instruction.Create(Code.Sar_rm8_imm8, Register.CL, 0x62);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rcx = ctx.MkBV(0x00000001096302F9, 64),
        };

        var reader = new ShellCodeReader(64, 0, [inst]);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var cfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Cf, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(0x00000001096302FE, rcxExpr.BigInteger);
    }

    [Fact]
    public void TranslateShl_rm32_imm8()
    {
        var inst = Instruction.Create(Code.Shl_rm32_imm8, Register.ESI, 0xE1);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rsi = ctx.MkBV(0x0000000000008000, 64),
            Zf = ctx.MkFalse(),
            Pf = ctx.MkTrue(),
            Of = ctx.MkTrue(),
            Sf = ctx.MkFalse(),
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
        var rsiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rsi, true);

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x0000000000010000, rsiExpr.BigInteger);
    }

    [Fact]
    public void TranslateShrd_rm64_r64_imm8_0()
    {
        var inst = Instruction.Create(Code.Shrd_rm64_r64_imm8, Register.RAX, Register.RBX, 4);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x0000000000010000, 64),
            Rbx = ctx.MkBV(0x0000000000010000, 64),
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
        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x0000000000001000, raxExpr.BigInteger);
        Assert.Equal(0x0000000000010000, rbxExpr.BigInteger);
    }

    [Fact]
    public void TranslateShrd_rm64_r64_imm8_1()
    {
        var inst = Instruction.Create(Code.Shrd_rm64_r64_imm8, Register.RAX, Register.RBX, 4);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x0248c9d68b352ba5, 64),
            Rbx = ctx.MkBV(0xa7f0da4c47c2d859, 64),
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
        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, ofExpr.BoolValue);
        Assert.Equal(0x812464eb459a95d2, raxExpr.BigInteger);
        Assert.Equal(0xa7f0da4c47c2d859, rbxExpr.BigInteger);
    }
}