using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.X86.SymbolicExecution.InstructionTranslate;

public class TranslateAddTest
{
    [Fact]
    public void TranslateAdd_AddNegative()
    {
        var value = 0xFFFFFE5C; // -420
        var inst = Instruction.Create(Code.Add_rm32_imm32, Register.EAX, value);
        var ctx = new Context();
        var initialContext = new RegisterContext(ctx, ctx.MkBV(0, 64), ctx.MkBV(0, 64))
        {
            Rax = ctx.MkBV(0, 64)
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

        Assert.Equal(Z3_lbool.Z3_L_FALSE, cfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, pfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, afExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, zfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_TRUE, sfExpr.BoolValue);
        Assert.Equal(Z3_lbool.Z3_L_FALSE, ofExpr.BoolValue);
        Assert.Equal(value, raxExpr.BigInteger);
    }
}