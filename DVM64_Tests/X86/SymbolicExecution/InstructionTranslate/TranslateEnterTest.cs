using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateEnterTest
{
    [Fact]
    public void TranslateS8_Add_Enter_Test()
    {
        List<Instruction> instStream =
        [
            Instruction.Create(Code.Push_r64, Register.R14),
            Instruction.Create(Code.Mov_r64_imm64, Register.R14, 0xFC921835989BED9D),
            Instruction.Create(Code.Pushfq),
            Instruction.Create(Code.Sub_rm16_imm16, Register.R14W, 0x3F03),
            Instruction.Create(Code.Or_rm16_r16, Register.R14W, Register.R14W),
            Instruction.Create(Code.Mov_r64_rm64, Register.R14,
                new MemoryOperand(Register.RSP, Register.None, 1, 8, 1)),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 8, 1),
                -1088446942),
            Instruction.Create(Code.Push_rm64, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Popfq),
            Instruction.Create(Code.Lea_r64_m, Register.RSP, new MemoryOperand(Register.RSP, Register.None, 1, 8, 1)),
            Instruction.Create(Code.Sub_rm64_imm8, Register.RSP, 0x8),
            Instruction.Create(Code.Push_r64, Register.R14),
            Instruction.Create(Code.Sub_rm64_imm8, Register.RSP, 0x8),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP), Register.R12),
            Instruction.Create(Code.Pushq_imm32, 0x6005B3B8),
            Instruction.Create(Code.Pushfq),
            Instruction.Create(Code.Pop_rm64, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.None, 1, 0x18, 1),
                Register.RDX),
            Instruction.Create(Code.Pushq_imm32, 0xF9DDC85),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP), Register.RBX),
            Instruction.Create(Code.Mov_r32_imm32, Register.EBX, 0x9239F600),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Lea_r64_m, Register.RAX,
                new MemoryOperand(Register.None, Register.RBX, 4, -0x2FCF91C4, 1)),
            Instruction.Create(Code.Push_r64, Register.R8),
            Instruction.Create(Code.Not_rm8, Register.BL),
            Instruction.Create(Code.Push_r64, Register.RDX),
            Instruction.Create(Code.Btc_rm64_r64, Register.RBX, Register.RAX),
            Instruction.Create(Code.Neg_rm32, Register.EAX),
            Instruction.Create(Code.Push_r64, Register.R11),
            Instruction.Create(Code.Push_r64, Register.R9),
            Instruction.Create(Code.Mov_rm32_imm32, Register.R9D, 0x8000000),
            Instruction.Create(Code.Shl_rm64_imm8, Register.R9, 5),
            Instruction.Create(Code.Mov_rm32_r32, Register.R8D, Register.EAX),
            Instruction.Create(Code.Not_rm64, Register.RAX),
            Instruction.Create(Code.Push_r64, Register.R15),
            Instruction.Create(Code.Push_r64, Register.RCX),
            Instruction.Create(Code.Push_r64, Register.R8),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP), Register.R10),
            Instruction.Create(Code.Push_r64, Register.R13),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EDX, Register.R8L),
            Instruction.Create(Code.Mov_r64_imm64, Register.R10, 0x00007FF5F3BC0000),
            Instruction.Create(Code.Shl_rm64_imm8, Register.RBX, 0xB9),
            Instruction.Create(Code.Add_rm64_imm32, Register.RDX, 0x2F2DEA31),
            Instruction.Create(Code.Push_r64, Register.RBP),
            Instruction.Create(Code.Inc_rm32, Register.EBX),
            Instruction.Create(Code.Mov_r32_imm32, Register.ECX, 0xAF38761B),
            Instruction.Create(Code.Adc_rm8_imm8, Register.BL, 0xB1),
            Instruction.Create(Code.Push_r64, Register.RDI),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 4, -0x288, 1),
                Register.R10),
            Instruction.Create(Code.Mov_r32_rm32, Register.R11D,
                new MemoryOperand(Register.RSP, Register.RBX, 4, -0x248, 1)),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EBP, Register.DL),
            Instruction.Create(Code.Shl_rm32_CL, Register.ECX, Register.CL),
            Instruction.Create(Code.Not_rm32, Register.R11D),
            Instruction.Create(Code.Inc_rm16, Register.DX),
            Instruction.Create(Code.Rol_rm16_imm8, Register.BP, 0xAD),
            Instruction.Create(Code.Xor_rm32_imm32, Register.R11D, 0xD00A912),
            Instruction.Create(Code.Cwd),
            Instruction.Create(Code.Ror_rm32_1, Register.R11D, 1),
            Instruction.Create(Code.And_rm8_r8, Register.BPL, Register.BPL),
            Instruction.Create(Code.Lea_r32_m, Register.R11D,
                new MemoryOperand(Register.R11, Register.R8, 1, -0x4DD778B4, 1)),
            Instruction.Create(Code.Adc_rm32_imm32, Register.R8D, 0x449C689A),
            Instruction.Create(Code.Adc_rm16_imm16, Register.CX, 0x27AA),
            Instruction.Create(Code.Adc_rm64_r64, Register.R11, Register.R10),
            Instruction.Create(Code.Movzx_r32_rm8, Register.R10D, Register.CL),
            Instruction.Create(Code.Mov_rm32_r32, Register.EDI, Register.EBP),
            Instruction.Create(Code.Add_rm64_r64, Register.RDX, Register.RDI),
            Instruction.Create(Code.Adc_rm64_r64, Register.R11, Register.R9),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 4, -0x248, 1),
                Register.RSI),
            Instruction.Create(Code.Mov_rm64_r64, Register.RDI, Register.RSP),
            Instruction.Create(Code.Sub_rm8_r8, Register.R10L, Register.R8L),
            Instruction.Create(Code.Lea_r64_m, Register.R9,
                new MemoryOperand(Register.None, Register.R8, 4, 0x353DC2B7, 1)),
            Instruction.Create(Code.Lea_r64_m, Register.R8,
                new MemoryOperand(Register.RDI, Register.RBX, 4, -0x510, 1)),
            Instruction.Create(Code.Mul_rm32, Register.R10D),
            Instruction.Create(Code.Xadd_rm32_r32, Register.ECX, Register.EBX),
            Instruction.Create(Code.And_rm8_imm8, Register.R8L, 0xF0),
            Instruction.Create(Code.Neg_rm8, Register.R9L),
            Instruction.Create(Code.Mov_rm64_r64, Register.RSP, Register.R8),
            Instruction.Create(Code.Mov_rm32_imm32, Register.ESI, 0xDA8175A2),
            Instruction.Create(Code.Mov_rm64_r64, Register.R9, Register.R11),
            Instruction.Create(Code.Movsx_r32_rm8, Register.R10D, Register.SIL),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EBX, Register.SIL),
            Instruction.Create(Code.Movsx_r32_rm16, Register.ECX, Register.R10W),
            Instruction.Create(Code.Mov_r64_imm64, Register.RBX, 0x7FF5F3BC0000),
            Instruction.Create(Code.Movzx_r32_rm8, Register.EDX, Register.CL),
            Instruction.Create(Code.Dec_rm64, Register.R10),
            Instruction.Create(Code.Sub_rm64_r64, Register.R9, Register.RBX),
            Instruction.Create(Code.Mov_rm32_imm32, Register.EDX, 0xF7136686),
            Instruction.Create(Code.Sub_rm64_imm8, Register.RSP, 0x8),
            Instruction.Create(Code.Mov_r64_imm64, Register.R10, 0x00007FF733BE241D), // lea r10, [00007FF733BE241D]
            Instruction.Create(Code.Sub_rm64_imm8, Register.R11, 0x4),
            Instruction.Create(Code.Push_r64, Register.RDX)
        ];
        var ctx = new Context();

        var initialContext = new RegisterContext(ctx, ctx.MkBV(0x000000EB7C6FF938, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x0000000000000023, 64),
            Rbx = ctx.MkBV(0x000002A833B2A8B0, 64),
            Rcx = ctx.MkBV(0x0000000000000001, 64),
            Rdx = ctx.MkBV(0x0000000000000002, 64),
            Rbp = ctx.MkBV(0x000000EB7C6FF980, 64),
            Rsi = ctx.MkBV(0x0000000000000000, 64),
            Rdi = ctx.MkBV(0x000002A833B2F4C0, 64),
            R8 = ctx.MkBV(0x00007FFA2D970990, 64),
            R9 = ctx.MkBV(0x0000000000000068, 64),
            R10 = ctx.MkBV(0x0000000000000000, 64),
            R11 = ctx.MkBV(0x000000EB7C6FF7D0, 64),
            R12 = ctx.MkBV(0x0000000000000000, 64),
            R13 = ctx.MkBV(0x0000000000000000, 64),
            R14 = ctx.MkBV(0x0000000000000000, 64),
            R15 = ctx.MkBV(0x0000000000000000, 64),
            Rip = ctx.MkBV(0x00007FF733BC1030, 64),
            If = ctx.MkBool(true)
        };

        var reader = new ShellCodeReader(64, 0x00007FF7FB210000, instStream);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in instStream)
        {
            translate.TranslateInstruction(inst, ref initialContext);
        }

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var cfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Cf, true);
        var pfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Pf, true);
        var afExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Af, true);
        var zfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Zf, true);
        var sfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Sf, true);
        var tfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Tf, true);
        var ifExpr = (BoolExpr)solver.Model.Evaluate(initialContext.If, true);
        var dfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Df, true);
        var ofExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Of, true);
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

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, tfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, ifExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, dfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x000000008C4D1FBF, raxExpr.BigInteger);
        Assert.Equal(0x00007FF5F3BC0000, rbxExpr.BigInteger);
        Assert.Equal(0x00000000FFFFFFA2, rcxExpr.BigInteger);
        Assert.Equal(0x00000000F7136686, rdxExpr.BigInteger);
        Assert.Equal(0x00000000FFFFBFFE, rbpExpr.BigInteger);
        Assert.Equal(0x000000EB7C6FF650, rspExpr.BigInteger);
        Assert.Equal(0x00000000DA8175A2, rsiExpr.BigInteger);
        Assert.Equal(0x000000EB7C6FF8B0, rdiExpr.BigInteger);
        Assert.Equal(0x000000EB7C6FF660, r8Expr.BigInteger);
        Assert.Equal(0x000000014000A577, r9Expr.BigInteger);
        Assert.Equal(0x00007FF733BE241D, r10Expr.BigInteger);
        Assert.Equal(0x00007FF733BCA573, r11Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r12Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r13Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r14Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r15Expr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000002, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBVConst("init_rflags", 64),
            ctx.MkBV(0x000002A833B2A8B0, 64),
            ctx.MkBV(0x0000000000000023, 64),
            ctx.MkBV(0x00007FFA2D970990, 64),
            ctx.MkBV(0x00007FF5F3BC0000, 64),
            ctx.MkBV(0x000000EB7C6FF7D0, 64),
            ctx.MkBV(0x0000000000000068, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000001, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x000000EB7C6FF980, 64),
            ctx.MkBV(0x000002A833B2F4C0, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x00000000F7136686, 64),
        }, initialContext.Stack);
    }

    [Fact]
    public void TranslateS16_Add_Enter_Test()
    {
        List<Instruction> instStream =
        [
            // START OF VM ENTER
            Instruction.Create(Code.Push_r64, Register.RDX),
            Instruction.Create(Code.Pushfq),
            Instruction.Create(Code.Mov_r64_imm64, Register.RDX, 0xEB32DD20D58E94B9),
            Instruction.Create(Code.Bswap_r32, Register.EDX),
            Instruction.Create(Code.Push_r64, Register.RSI),
            Instruction.Create(Code.Mov_r64_imm64, Register.RSI, 0x131104A892BB5797),
            Instruction.Create(Code.Xor_rm64_imm32, Register.RSI, -1500755649),
            Instruction.Create(Code.Or_rm32_imm32, Register.EDX, 0xD1AF241A),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x14000B08B),
            Instruction.Create(Code.Mov_r64_rm64, Register.RDX,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x18, 1)),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.None, 1, 0x18, 1),
                -1088440280),
            Instruction.Create(Code.Mov_r64_rm64, Register.RSI,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x8, 1)),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x14001ABB3),
            Instruction.Create(Code.Push_rm64, new MemoryOperand(Register.RSP, Register.None, 1, 0x18, 1)),
            Instruction.Create(Code.Popfq),
            Instruction.Create(Code.Lea_r64_m, Register.RSP,
                new MemoryOperand(Register.RSP, Register.None, 1, 0x20, 1)),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x1400211C1),
            Instruction.Create(Code.Push_r64, Register.R14),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x1400244CF),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP), Register.R12),
            Instruction.Create(Code.Pushq_imm32, 0x6005B3B8),
            Instruction.Create(Code.Pushfq),
            Instruction.Create(Code.Pop_rm64, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.None, 1, 0x18, 1),
                Register.RDX),
            Instruction.Create(Code.Pushq_imm32, 0xF9DDC85),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP), Register.RBX),
            Instruction.Create(Code.Mov_r32_imm32, Register.EBX, 0x9239F600),
            Instruction.Create(Code.Push_r64, Register.RAX),
            Instruction.Create(Code.Lea_r64_m, Register.RAX,
                new MemoryOperand(Register.None, Register.RBX, 4, -0x2FCF91C4, 1)),
            Instruction.Create(Code.Push_r64, Register.R8),
            Instruction.Create(Code.Not_rm8, Register.BL),
            Instruction.Create(Code.Push_r64, Register.RDX),
            Instruction.Create(Code.Btc_rm64_r64, Register.RBX, Register.RAX),
            Instruction.Create(Code.Neg_rm32, Register.EAX),
            Instruction.Create(Code.Push_r64, Register.R11),
            Instruction.Create(Code.Push_r64, Register.R9),
            Instruction.Create(Code.Mov_rm32_imm32, Register.R9D, 0x8000000),
            Instruction.Create(Code.Shl_rm64_imm8, Register.R9, 5),
            Instruction.Create(Code.Mov_rm32_r32, Register.R8D, Register.EAX),
            Instruction.Create(Code.Not_rm64, Register.RAX),
            Instruction.Create(Code.Push_r64, Register.R15),
            Instruction.Create(Code.Push_r64, Register.RCX),
            Instruction.Create(Code.Push_r64, Register.R8),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP), Register.R10),
            Instruction.Create(Code.Push_r64, Register.R13),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EDX, Register.R8L),
            Instruction.Create(Code.Mov_r64_imm64, Register.R10, 0x00007FF608B60000),
            Instruction.Create(Code.Shl_rm64_imm8, Register.RBX, 0xB9),
            Instruction.Create(Code.Add_rm64_imm32, Register.RDX, 0x2F2DEA31),
            Instruction.Create(Code.Push_r64, Register.RBP),
            Instruction.Create(Code.Inc_rm32, Register.EBX),
            Instruction.Create(Code.Mov_r32_imm32, Register.ECX, 0xAF38761B),
            Instruction.Create(Code.Adc_rm8_imm8, Register.BL, 0xB1),
            Instruction.Create(Code.Push_r64, Register.RDI),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 4, -0x288, 1),
                Register.R10),
            Instruction.Create(Code.Mov_r32_rm32, Register.R11D,
                new MemoryOperand(Register.RSP, Register.RBX, 4, -0x248, 1)),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EBP, Register.DL),
            Instruction.Create(Code.Shl_rm32_CL, Register.ECX, Register.CL),
            Instruction.Create(Code.Not_rm32, Register.R11D),
            Instruction.Create(Code.Inc_rm16, Register.DX),
            Instruction.Create(Code.Rol_rm16_imm8, Register.BP, 0xAD),
            Instruction.Create(Code.Xor_rm32_imm32, Register.R11D, 0xD00A912),
            Instruction.Create(Code.Cwd),
            Instruction.Create(Code.Ror_rm32_1, Register.R11D, 1),
            Instruction.Create(Code.And_rm8_r8, Register.BPL, Register.BPL),
            Instruction.Create(Code.Lea_r32_m, Register.R11D,
                new MemoryOperand(Register.R11, Register.R8, 1, -0x4DD778B4, 1)),
            Instruction.Create(Code.Adc_rm32_imm32, Register.R8D, 0x449C689A),
            Instruction.Create(Code.Adc_rm16_imm16, Register.CX, 0x27AA),
            Instruction.Create(Code.Adc_rm64_r64, Register.R11, Register.R10),
            Instruction.Create(Code.Movzx_r32_rm8, Register.R10D, Register.CL),
            Instruction.Create(Code.Mov_rm32_r32, Register.EDI, Register.EBP),
            Instruction.Create(Code.Add_rm64_r64, Register.RDX, Register.RDI),
            Instruction.Create(Code.Adc_rm64_r64, Register.R11, Register.R9),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 4, -0x248, 1),
                Register.RSI),
            Instruction.Create(Code.Mov_rm64_r64, Register.RDI, Register.RSP),
            Instruction.Create(Code.Sub_rm8_r8, Register.R10L, Register.R8L),
            Instruction.Create(Code.Lea_r64_m, Register.R9,
                new MemoryOperand(Register.None, Register.R8, 4, 0x353DC2B7, 1)),
            Instruction.Create(Code.Lea_r64_m, Register.R8,
                new MemoryOperand(Register.RDI, Register.RBX, 4, -0x510, 1)),
            Instruction.Create(Code.Mul_rm32, Register.R10D),
            Instruction.Create(Code.Xadd_rm32_r32, Register.ECX, Register.EBX),
            Instruction.Create(Code.And_rm8_imm8, Register.R8L, 0xF0),
            Instruction.Create(Code.Neg_rm8, Register.R9L),
            Instruction.Create(Code.Mov_rm64_r64, Register.RSP, Register.R8),
            Instruction.Create(Code.Mov_rm32_imm32, Register.ESI, 0xDA8175A2),
            Instruction.Create(Code.Mov_rm64_r64, Register.R9, Register.R11),
            Instruction.Create(Code.Movsx_r32_rm8, Register.R10D, Register.SIL),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EBX, Register.SIL),
            Instruction.Create(Code.Movsx_r32_rm16, Register.ECX, Register.R10W),
            Instruction.Create(Code.Mov_r64_imm64, Register.RBX, 0x7FF608B60000),
            Instruction.Create(Code.Movzx_r32_rm8, Register.EDX, Register.CL),
            Instruction.Create(Code.Dec_rm64, Register.R10),
            Instruction.Create(Code.Sub_rm64_r64, Register.R9, Register.RBX),
            Instruction.Create(Code.Mov_rm32_imm32, Register.EDX, 0xF7136686),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x14001B590),
            Instruction.Create(Code.Mov_r64_imm64, Register.R10,
                0x00007FF748B8241D), // lea r10,qword ptr ds:[7FF748B8241D]
            Instruction.Create(Code.Sub_rm64_imm8, Register.R11, 0x4),
            Instruction.Create(Code.Push_r64, Register.RDX),
            Instruction.Create(Code.Mov_r32_imm32, Register.EBX, 0x00000000AC304672), // mov ebx, [r11]
            Instruction.CreateBranch(Code.Call_rel32_64, 0x14002DEA9),
            Instruction.Create(Code.Xor_r32_rm32, Register.EBX, Register.R9D),
            Instruction.Create(Code.Movzx_r32_rm8, Register.EAX, Register.DL),
            Instruction.Create(Code.Mul_rm16, Register.AX),
            Instruction.Create(Code.Shr_rm8_imm8, Register.AL, 0xE5),
            Instruction.Create(Code.Dec_rm32, Register.EBX),
            Instruction.Create(Code.Mov_rm64_imm32, new MemoryOperand(Register.RSP, Register.RAX, 1, -0x4601, 1),
                0x123CA026),
            Instruction.Create(Code.Rol_rm32_imm8, Register.EBX, 0x2),
            Instruction.Create(Code.Cbw),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RAX, 8, 8, 1), Register.RAX),
            Instruction.Create(Code.Cwde),
            Instruction.Create(Code.Neg_rm32, Register.EBX),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x140030BEB),
            Instruction.Create(Code.Ror_rm32_imm8, Register.EBX, 0x2),
            Instruction.Create(Code.Bswap_r32, Register.EBX),
            Instruction.Create(Code.Dec_rm8, new MemoryOperand(Register.RSP, Register.RAX, 8, 0x7, 1)),
            Instruction.Create(Code.Mov_r32_rm32, Register.ECX,
                new MemoryOperand(Register.RSP, Register.RAX, 2, 0xC, 1)),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RAX, 8, 0x10, 1),
                Register.R9),
            Instruction.Create(Code.Xor_rm32_r32, new MemoryOperand(Register.RSP, Register.RAX, 2, 0x16, 1),
                Register.EBX),
            Instruction.Create(Code.Mov_r64_rm64, Register.R9,
                new MemoryOperand(Register.RSP, Register.RAX, 8, 0x10, 1)),
            Instruction.Create(Code.Shl_rm32_imm8, Register.EDX, 0x89),
            Instruction.Create(Code.Add_r8_rm8, Register.DL, Register.AL),
            Instruction.Create(Code.Sbb_r32_rm32, Register.ECX, Register.EDX),
            Instruction.Create(Code.Movsxd_r64_rm32, Register.RBX, Register.EBX),
            Instruction.Create(Code.And_rm8_r8, new MemoryOperand(Register.RSP, Register.RDX, 1, -637534191, 1),
                Register.AH),
            Instruction.Create(Code.Pop_r64, Register.RAX),
            Instruction.Create(Code.Adc_rm64_r64, Register.R10, Register.RBX),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EBP, Register.DL),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RBP, 2, -2, 1),
                Register.R10),
            Instruction.Create(Code.Retnq_imm16, 0x10),
            // END OF VM ENTER

            // START OF FIRST HANDLER
            Instruction.Create(Code.Mov_rm32_imm32, Register.EAX, 0x4399468F),
            Instruction.Create(Code.Movsx_r32_rm16, Register.EBX, Register.AX),
            Instruction.Create(Code.Movsx_r32_rm16, Register.EDX, Register.BX),
            Instruction.Create(Code.Mov_r64_rm64, Register.RBP,
                new MemoryOperand(Register.RAX, Register.RDI, 1, -0x4399468F, 1)),
            Instruction.Create(Code.Lea_r64_m, Register.RBX,
                new MemoryOperand(Register.RDX, Register.RBX, 4, 0x6FAA14A9, 1)),
            Instruction.Create(Code.Sar_rm16_imm8, Register.DX, 0x81),
            Instruction.Create(Code.Lea_r64_m, Register.RDI,
                new MemoryOperand(Register.RDI, Register.RAX, 1, -0x43994687, 1)),
            Instruction.Create(Code.Lea_r64_m, Register.RSI,
                new MemoryOperand(Register.None, Register.RBX, 2, 0xD296192, 1)),
            Instruction.Create(Code.Movsx_r32_rm8, Register.R8D, Register.SIL),
            Instruction.Create(Code.Btr_rm64_r64, Register.RDX, Register.RBX),
            Instruction.Create(Code.Lea_r64_m, Register.R11,
                new MemoryOperand(Register.R11, Register.RAX, 1, -0x43994690, 1)),
            Instruction.Create(Code.Inc_rm16, Register.AX),
            Instruction.Create(Code.Rol_rm64_imm8, Register.RDX, 0xCA),
            Instruction.Create(Code.Sbb_rm32_imm32, Register.EBX, 0xB00B2B2B),
            Instruction.Create(Code.Mov_r8_imm8, Register.R8L, 0x97), // mov r8b,byte ptr ds:[rax+r11-43994690]
            Instruction.Create(Code.Xor_r8_rm8, Register.R8L, Register.R9L),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x14002FE23),
            Instruction.Create(Code.Ror_rm8_1, Register.BL, 1),
            Instruction.Create(Code.Adc_r8_rm8, Register.R8L, Register.DL),
            Instruction.Create(Code.Not_rm8, Register.R8L),
            Instruction.Create(Code.Shr_rm64_imm8, Register.RSI, 0x43),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RDX, 1, -0x8D1C00, 1),
                Register.RAX),
            Instruction.Create(Code.Rol_rm64_imm8, new MemoryOperand(Register.RSP, Register.RAX, 1, -0x43994690, 1),
                0x6F),
            Instruction.Create(Code.Xor_rm8_imm8, Register.R8L, 0x18),
            Instruction.Create(Code.Bts_rm16_r16, Register.BX, Register.SI),
            Instruction.Create(Code.Ror_rm8_1, Register.R8L, 1),
            Instruction.Create(Code.Xor_rm8_r8, Register.R9L, Register.R8L),
            Instruction.Create(Code.Inc_rm8, Register.SIL),
            Instruction.Create(Code.Lea_r64_m, Register.R8, new MemoryOperand(Register.RSP, Register.R8, 1, 0x8, 1)),
            Instruction.Create(Code.Sub_rm16_r16, new MemoryOperand(Register.RSP, Register.RAX, 1, -0x4399468B, 1),
                Register.DX),
            Instruction.Create(Code.Mov_rm32_imm32, Register.ECX, 0x2DAEC782),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RAX, Register.R8, 1, -0x43994690, 1),
                Register.RBP),
            Instruction.Create(Code.Ror_rm16_imm8, new MemoryOperand(Register.RSP, Register.RAX, 1, -0x4399468D, 1),
                0x61),
            Instruction.Create(Code.Lea_r64_m, Register.R11,
                new MemoryOperand(Register.R11, Register.RAX, 1, -0x43994694, 1)),
            Instruction.Create(Code.Not_rm16, Register.AX),
            Instruction.Create(Code.Lea_r64_m, Register.R8, new MemoryOperand(Register.None, Register.RBX, 8, -0x49D47EC3, 1)),
            Instruction.Create(Code.Mov_r32_imm32, Register.EAX, 0x000000000DA211BE), // mov eax,dword ptr ds:[rax+r11-4399B96F]
            Instruction.Create(Code.Pop_r64, Register.R8),
            Instruction.Create(Code.Movsx_r32_rm8, Register.EBP, Register.SIL),
            Instruction.Create(Code.Bt_rm32_r32, Register.EBP, Register.EBX),
            Instruction.Create(Code.Xor_r32_rm32, Register.EAX, Register.R9D),
            Instruction.Create(Code.Shl_rm32_CL, Register.R8D, Register.CL),
            Instruction.Create(Code.Not_rm32, Register.EAX),
            Instruction.Create(Code.Bt_rm16_imm8, Register.BX, 0xB6),
            Instruction.Create(Code.Ror_rm32_imm8, Register.EAX, 0x3),
            Instruction.Create(Code.Not_rm32, Register.EAX),
            Instruction.Create(Code.Sub_rm32_imm32, Register.ECX, 0x331201BB),
            Instruction.Create(Code.Sbb_rm8_r8, Register.R8L, Register.BPL),
            Instruction.Create(Code.Bswap_r32, Register.EAX),
            Instruction.Create(Code.Xchg_rm16_r16, Register.R8W, Register.BP),
            Instruction.Create(Code.Lea_r32_m, Register.EAX, new MemoryOperand(Register.RDX, Register.RAX, 1, -0x8390D8E, 1)),
            Instruction.Create(Code.Neg_rm32, Register.EAX),
            Instruction.Create(Code.Bt_rm32_imm8, Register.EBX, 0xBB),
            Instruction.Create(Code.Rol_rm32_imm8, Register.EAX, 2),
            Instruction.Create(Code.Dec_rm32, Register.ESI),
            Instruction.Create(Code.Lea_r32_m, Register.EAX, new MemoryOperand(Register.RAX, Register.RDX, 2, 0x386A1A16, 1)),
            Instruction.Create(Code.Sub_r32_rm32, Register.EDX, Register.R8D),
            Instruction.Create(Code.Rol_rm32_1, Register.EAX, 1),
            Instruction.Create(Code.Xadd_rm32_r32, Register.R8D, Register.EDX),
            Instruction.Create(Code.Neg_rm32, Register.EAX),
            Instruction.Create(Code.Push_rm64, Register.R9),
            Instruction.Create(Code.Xor_rm32_r32, new MemoryOperand(Register.RSP, Register.RDX, 1, -0xFF90, 1), Register.EAX),
            Instruction.Create(Code.Shl_rm16_imm8, Register.CX, 0xC4),
            Instruction.Create(Code.Shr_rm16_imm8, Register.SI, 0xEB),
            Instruction.Create(Code.Push_rm64, Register.RBP),
            Instruction.Create(Code.Mov_r64_rm64, Register.R9, new MemoryOperand(Register.RSP, Register.RDX, 1, -0xFF88, 1)),
            Instruction.Create(Code.Adc_r8_rm8, Register.DL, new MemoryOperand(Register.RSP, Register.RDX, 1, -0xFF8C, 1)),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x140046DE7),
            Instruction.Create(Code.Movsxd_r64_rm32, Register.RAX, Register.EAX),
            Instruction.Create(Code.Shl_rm16_imm8, Register.BP, 0xA2),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x140020934),
            Instruction.Create(Code.Setbe_rm8, Register.DL),
            Instruction.Create(Code.Adc_rm64_r64, Register.R10, Register.RAX),
            Instruction.Create(Code.Neg_rm8, Register.DL),
            Instruction.Create(Code.Sar_rm16_imm8, Register.SI, 0xA1),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RDX, 1, -0xFEE8, 1), Register.RBX),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RDX, 1, -0xFF00, 1), Register.R10),
            Instruction.Create(Code.Retnq_imm16, 0x18),
            // END OF FIRST HANDLER

            // START OF SECOND HANDLER
            Instruction.Create(Code.Mov_rm32_imm32, Register.ESI, 0x7E964B86),
            Instruction.Create(Code.Lea_r64_m, Register.R11, new MemoryOperand(Register.R11, Register.RSI, 1, -0x7E964B8A, 1)),
            Instruction.Create(Code.Lea_r64_m, Register.RDX, new MemoryOperand(Register.None, Register.RSI, 2, -0x23549BF0, 1)),
            Instruction.Create(Code.Mov_r32_imm32, Register.ESI, 0x00000000D88F6511), // mov esi,dword ptr ds:[r11+rsi-7E964B86]
            Instruction.Create(Code.Movzx_r32_rm16, Register.EBX, Register.DX),
            Instruction.Create(Code.Neg_rm16, Register.DX),
            Instruction.Create(Code.Xor_rm32_r32, Register.ESI, Register.R9D),
            Instruction.Create(Code.Lea_r64_m, Register.RCX, new MemoryOperand(Register.RDX, Register.RBX, 4, 0x2F88A72B, 1)),
            Instruction.Create(Code.Neg_rm16, Register.CX),
            Instruction.Create(Code.Not_rm32, Register.ESI),
            Instruction.Create(Code.Ror_rm32_imm8, Register.ESI, 0x3),
            Instruction.Create(Code.Not_rm8, Register.DL),
            Instruction.Create(Code.Mov_rm32_r32, Register.R8D, Register.ECX),
            Instruction.Create(Code.Mov_rm32_r32, Register.EBP, Register.EBX),
            Instruction.Create(Code.Not_rm32, Register.ESI),
            Instruction.Create(Code.Push_rm64, Register.RBP),
            Instruction.Create(Code.And_rm8_imm8, Register.DL, 0x93),
            Instruction.Create(Code.Bswap_r32, Register.ESI),
            Instruction.Create(Code.Xor_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 4, -0x3EC70, 1), Register.RBP),
            Instruction.Create(Code.Sar_rm64_CL, new MemoryOperand(Register.RSP, Register.RBX, 1, -0xFB1C, 1), Register.CL),
            Instruction.Create(Code.Lea_r32_m, Register.ESI, new MemoryOperand(Register.RSI, Register.RBX, 8, -0x7B3CA6E, 1)),
            Instruction.Create(Code.Btr_rm16_r16, Register.CX, Register.BP),
            Instruction.Create(Code.Or_r16_rm16, Register.DX, Register.CX),
            Instruction.Create(Code.Neg_rm32, Register.ESI),
            Instruction.Create(Code.Rol_rm32_imm8, Register.ESI, 0x2),
            Instruction.Create(Code.Or_rm8_r8, Register.R8L, Register.CL),
            Instruction.Create(Code.Sbb_rm16_imm16, Register.CX, 0x6488),
            Instruction.Create(Code.Lea_r32_m, Register.ESI, new MemoryOperand(Register.RSI, Register.RBX, 1, 0x398356FA, 1)),
            Instruction.Create(Code.Adc_r64_rm64, Register.RBP, new MemoryOperand(Register.RSP, Register.RBX, 2, -0x1F638, 1)),
            Instruction.Create(Code.Rol_rm32_1, Register.ESI, 1),
            Instruction.Create(Code.Sub_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 1, -0xFB1C, 1), Register.RCX),
            Instruction.Create(Code.Rol_rm16_imm8, Register.BX, 0x48),
            Instruction.Create(Code.Bt_rm16_imm8, Register.R8W, 0x38),
            Instruction.Create(Code.Neg_rm32, Register.ESI),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 1, -0x1CFB, 1), Register.R9),
            Instruction.Create(Code.Sar_rm8_imm8, Register.CL, 0x62),
            Instruction.Create(Code.Btr_rm16_r16, Register.CX, Register.BP),
            Instruction.Create(Code.Xor_rm32_r32, new MemoryOperand(Register.RSP, Register.RBX, 4, -0x73EC, 1), Register.ESI),
            Instruction.Create(Code.Rol_rm16_imm8, Register.DX, 0xA8),
            Instruction.Create(Code.Pop_r64, Register.R9),
            Instruction.Create(Code.Movsxd_r64_rm32, Register.RSI, Register.ESI),
            Instruction.CreateBranch(Code.Call_rel32_64, 0x14004B863),
            Instruction.Create(Code.Add_rm64_r64, Register.R10, Register.RSI),
            Instruction.Create(Code.Mov_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 8, -0xE7D8, 1), Register.RBX),
            Instruction.Create(Code.Dec_rm32, Register.EDX),
            Instruction.Create(Code.Mov_r32_rm32, Register.EAX, Register.ECX),
            Instruction.Create(Code.Mov_r64_rm64, Register.RBP, new MemoryOperand(Register.RDI, Register.RBX, 1, -0x1CFB, 1)),
            Instruction.Create(Code.Lea_r64_m, Register.RDI, new MemoryOperand(Register.RBX, Register.RDI, 1, -0x1CF3, 1)),
            Instruction.Create(Code.Movsx_r32_rm8, Register.ESI, Register.DL),
            Instruction.Create(Code.Not_rm32, new MemoryOperand(Register.RSP, Register.R8, 4, -0x258D9E01, 1)),
            Instruction.Create(Code.Lea_r64_m, Register.R11, new MemoryOperand(Register.R11, Register.RBX, 2, -0x39F7, 1)),
            Instruction.Create(Code.Xchg_rm32_r32, new MemoryOperand(Register.RSP, Register.RBX, 1, -0x1CF7, 1), Register.ECX),
            Instruction.Create(Code.Xchg_rm64_r64, new MemoryOperand(Register.RSP, Register.RBX, 1, -0x1CFB, 1), Register.R8),
            Instruction.Create(Code.Mov_r32_imm32, Register.EAX, 0x00000000000000E2), // movzx eax,byte ptr ds:[r11+rbx-1CFB]
            Instruction.Create(Code.Inc_rm16, new MemoryOperand(Register.RSP, Register.RBX, 2, -0x39F2, 1)),
        ];
        var ctx = new Context();

        var initialContext = new RegisterContext(ctx, ctx.MkBV(0x0000003F494FFE88, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x0000000000000022, 64),
            Rbx = ctx.MkBV(0x000001DA6A1FA8B0, 64),
            Rcx = ctx.MkBV(0x0000000000000001, 64),
            Rdx = ctx.MkBV(0x0000000000000002, 64),
            Rbp = ctx.MkBV(0x0000003F494FFED0, 64),
            Rsi = ctx.MkBV(0x0000000000000000, 64),
            Rdi = ctx.MkBV(0x000001DA6A1FF4C0, 64),
            R8 = ctx.MkBV(0x00007FFA2D970990, 64),
            R9 = ctx.MkBV(0x0000000000000068, 64),
            R10 = ctx.MkBV(0x0000000000000000, 64),
            R11 = ctx.MkBV(0x0000003F494FFD20, 64),
            R12 = ctx.MkBV(0x0000000000000000, 64),
            R13 = ctx.MkBV(0x0000000000000000, 64),
            R14 = ctx.MkBV(0x0000000000000000, 64),
            R15 = ctx.MkBV(0x0000000000000000, 64),
            If = ctx.MkBool(true)
        };

        var reader = new ShellCodeReader(64, 0x00007FF7FB210000, instStream);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in instStream)
        {
            translate.TranslateInstruction(inst, ref initialContext);
        }

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var cfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Cf, true);
        var pfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Pf, true);
        var afExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Af, true);
        var zfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Zf, true);
        var sfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Sf, true);
        var tfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Tf, true);
        var ifExpr = (BoolExpr)solver.Model.Evaluate(initialContext.If, true);
        var dfExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Df, true);
        var ofExpr = (BoolExpr)solver.Model.Evaluate(initialContext.Of, true);
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

        Assert.Equal(Z3_lbool.Z3_L_TRUE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, tfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, ifExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, dfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(0x00000000000000E2, raxExpr.BigInteger);
        Assert.Equal(0x0000000000001CFB, rbxExpr.BigInteger);
        Assert.Equal(0x0000000000FFFFFF, rcxExpr.BigInteger);
        Assert.Equal(0x00000000D9D79366, rdxExpr.BigInteger);
        Assert.Equal(0x0000003F494FFED0, rbpExpr.BigInteger);
        Assert.Equal(0x0000003F494FFBA8, rspExpr.BigInteger);
        Assert.Equal(0x0000000000000066, rsiExpr.BigInteger);
        Assert.Equal(0x0000003F494FFE10, rdiExpr.BigInteger);
        Assert.Equal(0x096302FEFF001CFB, r8Expr.BigInteger);
        Assert.Equal(0x0000000140004267, r9Expr.BigInteger);
        Assert.Equal(0x00007FF748B7ADDE, r10Expr.BigInteger);
        Assert.Equal(0x00007FF748B6B864, r11Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r12Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r13Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r14Expr.BigInteger);
        Assert.Equal(0x0000000000000000, r15Expr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000002, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBVConst("init_rflags", 64),
            ctx.MkBV(0x000001DA6A1FA8B0, 64),
            ctx.MkBV(0x0000000000000022, 64),
            ctx.MkBV(0x00007FFA2D970990, 64),
            ctx.MkBV(0x00007FF608B60000, 64),
            ctx.MkBV(0x0000003F494FFD20, 64),
            ctx.MkBV(0x0000000000000068, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000001, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000003F494FFED0, 64),
            ctx.MkBV(0x000001DA6A1FF4C0, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x000001DA6A1FF4C0, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000000000000, 64),
            ctx.MkBV(0x0000000109636781, 64),
            ctx.MkBV(0x00000000FFFF879F, 64),
            ctx.MkBVConst("ret_addr_140046DE7", 64),
            ctx.MkBV(0x00007FF748B86A1C, 64),
        }, initialContext.Stack);
    }
}