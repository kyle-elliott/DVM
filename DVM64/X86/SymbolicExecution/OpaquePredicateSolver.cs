using Iced.Intel;
using Microsoft.Z3;

namespace DVM64.X86.SymbolicExecution;

internal class OpaquePredicateSolver(Context ctx)
{
    private static string GetName() => "OpaquePredicateSolver";

    internal enum OpaquePredKind
    {
        NotTaken,
        Taken,
        NotOpaque,
        Error
    }

    internal OpaquePredKind SolveForJcc(Instruction instruction, ref RegisterContext state)
    {
        var flagExpr = instruction.Mnemonic switch
        {
            Mnemonic.Ja => ctx.MkAnd(ctx.MkEq(state.Cf, ctx.MkFalse()), ctx.MkEq(state.Zf, ctx.MkFalse())),
            Mnemonic.Jae => ctx.MkEq(state.Cf, ctx.MkFalse()),
            Mnemonic.Jb => ctx.MkEq(state.Cf, ctx.MkTrue()),
            Mnemonic.Jbe => ctx.MkOr(ctx.MkEq(state.Cf, ctx.MkTrue()), ctx.MkEq(state.Zf, ctx.MkTrue())),
            Mnemonic.Je => ctx.MkEq(state.Zf, ctx.MkTrue()),
            Mnemonic.Jg => ctx.MkAnd(ctx.MkEq(state.Zf, ctx.MkFalse()), ctx.MkEq(state.Sf, state.Of)),
            Mnemonic.Jge => ctx.MkEq(state.Sf, state.Of),
            Mnemonic.Jl => ctx.MkDistinct(state.Sf, state.Of),
            Mnemonic.Jle => ctx.MkOr(ctx.MkEq(state.Zf, ctx.MkTrue()), ctx.MkDistinct(state.Sf, state.Of)),
            Mnemonic.Jne => ctx.MkEq(state.Zf, ctx.MkFalse()),
            Mnemonic.Jno => ctx.MkEq(state.Of, ctx.MkFalse()),
            Mnemonic.Jnp => ctx.MkEq(state.Pf, ctx.MkFalse()),
            Mnemonic.Jns => ctx.MkEq(state.Sf, ctx.MkFalse()),
            Mnemonic.Jo => ctx.MkEq(state.Of, ctx.MkTrue()),
            Mnemonic.Jp => ctx.MkEq(state.Pf, ctx.MkTrue()),
            Mnemonic.Js => ctx.MkEq(state.Sf, ctx.MkTrue()),
            Mnemonic.Jcxz or Mnemonic.Jecxz or Mnemonic.Jrcxz => throw new NotImplementedException(
                $"Solving for {instruction.ToString()} is not supported yet!"),
            _ => throw new NotImplementedException($"Unsupported JCC: {instruction.ToString()}")
        };

        var solver = ctx.MkSolver();
        solver.Add(flagExpr);

        return solver.Check() switch
        {
            Status.UNSATISFIABLE => OpaquePredKind.NotTaken,
            Status.UNKNOWN => OpaquePredKind.NotOpaque,
            Status.SATISFIABLE => OpaquePredKind.Taken,
            _ => throw new ArgumentOutOfRangeException()
        };
    }
}