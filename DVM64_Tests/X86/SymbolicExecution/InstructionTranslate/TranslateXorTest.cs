using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateXorTest
{
    [Fact]
    public void TranslateXor_SingleMem()
    {
        var inst = Instruction.Create(Code.Xor_rm32_imm32,
            new MemoryOperand(Register.RSP, Register.None, 1, 0x8, 1),
            0x37903EA5);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64));
        initialContext.Stack.PushExpression(ctx.MkBV(0xFFFFFFFF928AC6AB, 64));
        initialContext.Stack.PushExpression(ctx.MkBV(0x0000000000000206, 64));

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

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);

        Assert.Equal(new List<BitVecExpr>
        {
            ctx.MkBVConst("red_zone_0", 64),
            ctx.MkBVConst("red_zone_1", 64),
            ctx.MkBVConst("red_zone_2", 64),
            ctx.MkBVConst("red_zone_3", 64),
            ctx.MkBVConst("init_ret_addr", 64),
            ctx.MkBV(0xFFFFFFFFA51AF80E, 64),
            ctx.MkBV(0x0000000000000206, 64)
        }, initialContext.Stack);
    }
}