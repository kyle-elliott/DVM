using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateMiscTest
{
    [Fact]
    public void TranslateHandler_StackAlias()
    {
        List<Instruction> insts =
        [
            Instruction.Create(Code.Pushq_imm32, -593383521),
            Instruction.Create(Code.Sub_rm64_imm8, Register.R11, 1),
            Instruction.Create(Code.Mov_r32_rm32, Register.ECX, new MemoryOperand(Register.RSP, Register.None, 1, 0x3, 1)),
            Instruction.Create(Code.Movsx_r32_rm16, Register.EAX, new MemoryOperand(Register.RSP)),
            Instruction.Create(Code.Cwde),
        ];

        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0x5F1000000007C378, 64),
            Rcx = ctx.MkBV(0x00000000000019D8, 64),
            R11 = ctx.MkBV(0x00007FF733BC92B1, 64)
        };

        var reader = new ShellCodeReader(64, 0, insts);
        var translate = new DVM64.X86.SymbolicExecution.InstructionTranslate(reader, ctx);

        foreach (var inst in insts)
            translate.TranslateInstruction(inst, ref initialContext);

        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());

        var raxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rax, true);
        var rcxExpr = (BitVecNum)solver.Model.Evaluate(initialContext.Rcx, true);
        var r11Expr = (BitVecNum)solver.Model.Evaluate(initialContext.R11, true);

        Assert.Equal(0x00000000FFFFAF9F, raxExpr.BigInteger);
        Assert.Equal(0x00000000FFFFFFDC, rcxExpr.BigInteger);
        Assert.Equal(0x00007FF733BC92B0, r11Expr.BigInteger);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0xFFFFFFFFDCA1AF9F, 64),
        }, initialContext.Stack);
    }
}