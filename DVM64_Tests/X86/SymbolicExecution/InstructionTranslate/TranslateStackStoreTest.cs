using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateStackStoreTest
{
    [Fact]
    public void TranslateMov_rm8_imm8()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 0, 0), 0x11),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 1, 1), 0x22),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 2, 1), 0x33),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 3, 1), 0x44),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 4, 1), 0x55),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 5, 1), 0x66),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 6, 1), 0x77),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 7, 1), 0x88),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 8, 1), 0x99),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 9, 1), 0xAA),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 10, 1), 0xBB),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 11, 1), 0xCC),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 12, 1), 0xDD),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 13, 1), 0xEE),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 14, 1), 0xFF),
            Instruction.Create(Code.Mov_rm8_imm8, new MemoryOperand(Register.RSP, Register.None, 1, 15, 1), 0x00),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);

        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x00FFEEDDCCBBAA99, 64),
            ctx.MkBV(0x8877665544332211, 64),
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateMov_rm16_imm16()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 0, 0), 0x11),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 1, 1), 0x22),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 2, 1), 0x33),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 3, 1), 0x44),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 4, 1), 0x55),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 5, 1), 0x66),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 6, 1), 0x77),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 7, 1), 0x88),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 8, 1), 0x99),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 9, 1), 0xAA),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 10, 1), 0xBB),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 11, 1), 0xCC),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 12, 1), 0xDD),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 13, 1), 0xEE),
            Instruction.Create(Code.Mov_rm16_imm16, new MemoryOperand(Register.RSP, Register.None, 1, 14, 1), 0xFF),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);

        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x00FFEEDDCCBBAA99, 64),
            ctx.MkBV(0x8877665544332211, 64),
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateMov_rm32_imm32()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 0, 0), 0x11),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 1, 1), 0x22),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 2, 1), 0x33),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 3, 1), 0x44),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 4, 1), 0x55),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 5, 1), 0x66),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 6, 1), 0x77),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 7, 1), 0x88),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 8, 1), 0x99),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 9, 1), 0xAA),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 10, 1), 0xBB),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 11, 1), 0xCC),
            Instruction.Create(Code.Mov_rm32_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 12, 1), 0xDD),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);

        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x000000DDCCBBAA99, 64),
            ctx.MkBV(0x8877665544332211, 64),
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateMov_rm64_imm32()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 0, 0), 0x11),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 1, 1), 0x22),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 2, 1), 0x33),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 3, 1), 0x44),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 4, 1), 0x55),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 5, 1), 0x66),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 6, 1), 0x77),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 7, 1), 0x88),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 8, 1), 0x99),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);

        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x0000000000000099, 64),
            ctx.MkBV(0x8877665544332211, 64),
        }, initialContext.Stack);
    }
}