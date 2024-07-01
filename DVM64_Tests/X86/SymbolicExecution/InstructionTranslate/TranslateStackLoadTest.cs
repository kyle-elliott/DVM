using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateStackLoadTest
{
    [Fact]
    public void TranslateMov_r8_rm8()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x1122334455667788),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x99AABBCCDDEEFF00),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Mov_r8_rm8, Register.AL, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Mov_r8_rm8, Register.BL, new MemoryOperand(Register.RSP, Register.None, 1, 0x1, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.CL, new MemoryOperand(Register.RSP, Register.None, 1, 0x2, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.DL, new MemoryOperand(Register.RSP, Register.None, 1, 0x3, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.BPL,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x4, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.SIL,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x5, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.DIL,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x6, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R8L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x7, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R9L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x8, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R10L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x9, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R11L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xA, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R12L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xB, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R13L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xC, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R14L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xD, 1)),
            Instruction.Create(Code.Mov_r8_rm8, Register.R15L,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xE, 1)),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);
        var rbpExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbp, true);
        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);
        var rsiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rsi, true);
        var rdiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdi, true);
        var r8Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R8, true);
        var r9Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R9, true);
        var r10Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R10, true);
        var r11Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R11, true);
        var r12Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R12, true);
        var r13Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R13, true);
        var r14Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R14, true);
        var r15Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R15, true);

        Assert.Equal(0x0000000000000000, raxExpr.BigInteger);
        Assert.Equal(0x00000000000000FF, rbxExpr.BigInteger);
        Assert.Equal(0x00000000000000EE, rcxExpr.BigInteger);
        Assert.Equal(0x00000000000000DD, rdxExpr.BigInteger);
        Assert.Equal(0x00000000000000CC, rbpExpr.BigInteger);
        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);
        Assert.Equal(0x00000000000000BB, rsiExpr.BigInteger);
        Assert.Equal(0x00000000000000AA, rdiExpr.BigInteger);
        Assert.Equal(0x0000000000000099, r8Expr.BigInteger);
        Assert.Equal(0x0000000000000088, r9Expr.BigInteger);
        Assert.Equal(0x0000000000000077, r10Expr.BigInteger);
        Assert.Equal(0x0000000000000066, r11Expr.BigInteger);
        Assert.Equal(0x0000000000000055, r12Expr.BigInteger);
        Assert.Equal(0x0000000000000044, r13Expr.BigInteger);
        Assert.Equal(0x0000000000000033, r14Expr.BigInteger);
        Assert.Equal(0x0000000000000022, r15Expr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x1122334455667788, 64),
            ctx.MkBV(0x99AABBCCDDEEFF00, 64),
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateMov_r16_rm16()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x1122334455667788),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x99AABBCCDDEEFF00),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Mov_r16_rm16, Register.AX, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Mov_r16_rm16, Register.BX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x1, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.CX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x2, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.DX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x3, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.BP,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x4, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.SI,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x5, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.DI,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x6, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R8W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x7, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R9W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x8, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R10W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x9, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R11W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xA, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R12W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xB, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R13W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xC, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R14W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xD, 1)),
            Instruction.Create(Code.Mov_r16_rm16, Register.R15W,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xE, 1)),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);
        var rbpExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbp, true);
        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);
        var rsiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rsi, true);
        var rdiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdi, true);
        var r8Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R8, true);
        var r9Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R9, true);
        var r10Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R10, true);
        var r11Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R11, true);
        var r12Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R12, true);
        var r13Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R13, true);
        var r14Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R14, true);
        var r15Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R15, true);

        Assert.Equal(0x000000000000FF00, raxExpr.BigInteger);
        Assert.Equal(0x000000000000EEFF, rbxExpr.BigInteger);
        Assert.Equal(0x000000000000DDEE, rcxExpr.BigInteger);
        Assert.Equal(0x000000000000CCDD, rdxExpr.BigInteger);
        Assert.Equal(0x000000000000BBCC, rbpExpr.BigInteger);
        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);
        Assert.Equal(0x000000000000AABB, rsiExpr.BigInteger);
        Assert.Equal(0x00000000000099AA, rdiExpr.BigInteger);
        Assert.Equal(0x0000000000008899, r8Expr.BigInteger);
        Assert.Equal(0x0000000000007788, r9Expr.BigInteger);
        Assert.Equal(0x0000000000006677, r10Expr.BigInteger);
        Assert.Equal(0x0000000000005566, r11Expr.BigInteger);
        Assert.Equal(0x0000000000004455, r12Expr.BigInteger);
        Assert.Equal(0x0000000000003344, r13Expr.BigInteger);
        Assert.Equal(0x0000000000002233, r14Expr.BigInteger);
        Assert.Equal(0x0000000000001122, r15Expr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x1122334455667788, 64),
            ctx.MkBV(0x99AABBCCDDEEFF00, 64),
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateMov_r32_rm32()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x1122334455667788),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x99AABBCCDDEEFF00),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Mov_r32_rm32, Register.EAX, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Mov_r32_rm32, Register.EBX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x1, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.ECX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x2, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.EDX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x3, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.EBP,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x4, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.ESI,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x5, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.EDI,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x6, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R8D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x7, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R9D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x8, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R10D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x9, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R11D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xA, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R12D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xB, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R13D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xC, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R14D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xD, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.R15D,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xE, 1)),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);
        var rbpExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbp, true);
        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);
        var rsiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rsi, true);
        var rdiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdi, true);
        var r8Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R8, true);
        var r9Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R9, true);
        var r10Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R10, true);
        var r11Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R11, true);
        var r12Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R12, true);
        var r13Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R13, true);
        var r14Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R14, true);
        var r15Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R15, true);

        Assert.Equal(0x00000000DDEEFF00, raxExpr.BigInteger);
        Assert.Equal(0x00000000CCDDEEFF, rbxExpr.BigInteger);
        Assert.Equal(0x00000000BBCCDDEE, rcxExpr.BigInteger);
        Assert.Equal(0x00000000AABBCCDD, rdxExpr.BigInteger);
        Assert.Equal(0x0000000099AABBCC, rbpExpr.BigInteger);
        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);
        Assert.Equal(0x000000008899AABB, rsiExpr.BigInteger);
        Assert.Equal(0x00000000778899AA, rdiExpr.BigInteger);
        Assert.Equal(0x0000000066778899, r8Expr.BigInteger);
        Assert.Equal(0x0000000055667788, r9Expr.BigInteger);
        Assert.Equal(0x0000000044556677, r10Expr.BigInteger);
        Assert.Equal(0x0000000033445566, r11Expr.BigInteger);
        Assert.Equal(0x0000000022334455, r12Expr.BigInteger);
        Assert.Equal(0x0000000011223344, r13Expr.BigInteger);
        Assert.Equal(0x0000000000112233, r14Expr.BigInteger);
        Assert.Equal(0x0000000000001122, r15Expr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x1122334455667788, 64),
            ctx.MkBV(0x99AABBCCDDEEFF00, 64),
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateMov_r64_rm64()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x1122334455667788),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x99AABBCCDDEEFF00),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Mov_r64_imm64, Register.RAX, 0x0000000000000000),
            Instruction.Create(Code.Mov_r64_rm64, Register.RAX, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Mov_r64_rm64, Register.RBX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x1, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.RCX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x2, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.RDX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x3, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.RBP,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x4, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.RSI,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x5, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.RDI,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x6, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R8,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x7, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R9,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x8, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R10,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x9, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R11,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xA, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R12,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xB, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R13,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xC, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R14,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xD, 1)),
            Instruction.Create(Code.Mov_r64_rm64, Register.R15,
                new MemoryOperand(Register.RSP, Register.None, 1, 0xE, 1)),
        ];
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(-0x8, 64), ctx.MkBV(0, 64));

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rbxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbx, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);
        var rdxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdx, true);
        var rbpExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rbp, true);
        var rspExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Stack.StackPointer, true);
        var rsiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rsi, true);
        var rdiExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rdi, true);
        var r8Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R8, true);
        var r9Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R9, true);
        var r10Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R10, true);
        var r11Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R11, true);
        var r12Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R12, true);
        var r13Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R13, true);
        var r14Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R14, true);
        var r15Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R15, true);

        Assert.Equal(0x99AABBCCDDEEFF00, raxExpr.BigInteger);
        Assert.Equal(0x8899AABBCCDDEEFF, rbxExpr.BigInteger);
        Assert.Equal(0x778899AABBCCDDEE, rcxExpr.BigInteger);
        Assert.Equal(0x66778899AABBCCDD, rdxExpr.BigInteger);
        Assert.Equal(0x5566778899AABBCC, rbpExpr.BigInteger);
        Assert.Equal(0xFFFFFFFFFFFFFFE8, rspExpr.BigInteger);
        Assert.Equal(0x445566778899AABB, rsiExpr.BigInteger);
        Assert.Equal(0x33445566778899AA, rdiExpr.BigInteger);
        Assert.Equal(0x2233445566778899, r8Expr.BigInteger);
        Assert.Equal(0x1122334455667788, r9Expr.BigInteger);
        Assert.Equal(0x0011223344556677, r10Expr.BigInteger);
        Assert.Equal(0x0000112233445566, r11Expr.BigInteger);
        Assert.Equal(0x0000001122334455, r12Expr.BigInteger);
        Assert.Equal(0x0000000011223344, r13Expr.BigInteger);
        Assert.Equal(0x0000000000112233, r14Expr.BigInteger);
        Assert.Equal(0x0000000000001122, r15Expr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x1122334455667788, 64),
            ctx.MkBV(0x99AABBCCDDEEFF00, 64),
        }, initialContext.Stack);
    }
}