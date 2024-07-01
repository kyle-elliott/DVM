using DVM64.Extensions;
using DVM64.X86;
using DVM64.X86.SymbolicExecution;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64.Explorer;

public class Tracer
{
    internal Tracer(PositionCodeReader reader, ulong entry)
    {
        _ctx = new Context();
        _translate = new InstructionTranslate(reader, _ctx);
        _opaquePredSolver = new OpaquePredicateSolver(_ctx);
        ExecContext = new RegisterContext(_ctx, _ctx.MkBV(-0x8, 64), _ctx.MkBV(entry, 64));
    }

    private static string GetName() => "Tracer";

    private readonly Context _ctx;
    private readonly InstructionTranslate _translate;
    private readonly OpaquePredicateSolver _opaquePredSolver;
    internal RegisterContext ExecContext;

    internal bool TraceBlock(BasicBlock block)
    {
        var instructions = block.GetInstructions();

        for (var i = 0; i < instructions.Count; i++)
        {
            var instruction = instructions[i];

            switch (instruction.Mnemonic)
            {
                case Mnemonic.Ja:
                case Mnemonic.Jae:
                case Mnemonic.Jb:
                case Mnemonic.Jbe:
                case Mnemonic.Je:
                case Mnemonic.Jg:
                case Mnemonic.Jge:
                case Mnemonic.Jl:
                case Mnemonic.Jle:
                case Mnemonic.Jne:
                case Mnemonic.Jno:
                case Mnemonic.Jnp:
                case Mnemonic.Jns:
                case Mnemonic.Jo:
                case Mnemonic.Jp:
                case Mnemonic.Js:
                {
                    var predKind = _opaquePredSolver.SolveForJcc(instruction, ref ExecContext);

                    switch (predKind)
                    {
                        case OpaquePredicateSolver.OpaquePredKind.NotTaken:
                        {
                            Logger.Debug(GetName(),
                                $"0x{instruction.IP:X} : {instruction.ToString()} : Solved opaque predicate : {OpaquePredicateSolver.OpaquePredKind.NotTaken.ToString()}");

                            // Remove the JCC and insert all instructions from the notTaken branch, then set the current block's
                            // taken and notTaken block to the followed child's blocks
                            block.RemoveAt(i);

                            var notTakenBlock = block.GetBranchNotTaken();
                            if (notTakenBlock == null)
                                throw new InvalidOperationException(
                                    "Current block has a JCC instruction but the lifter didn't properly lift the notTaken block!");

                            block.AddRange(notTakenBlock.GetInstructions());
                            block.SetBranchNotTaken(notTakenBlock.GetBranchNotTaken());
                            block.SetBranchTaken(notTakenBlock.GetBranchTaken());

                            // Make sure the instruction at the current index gets run
                            i--;

                            break;
                        }

                        case OpaquePredicateSolver.OpaquePredKind.Taken:
                        {
                            Logger.Debug(GetName(),
                                $"0x{instruction.IP:X} : {instruction.ToString()} : Solved opaque predicate : {OpaquePredicateSolver.OpaquePredKind.Taken.ToString()}");

                            // Remove the JCC and insert all instructions from the taken branch, then set the current block's
                            // taken and notTaken block to the followed child's blocks
                            block.RemoveAt(i);

                            var takenBlock = block.GetBranchTaken();
                            if (takenBlock == null)
                                throw new InvalidOperationException(
                                    "Current block has a JCC instruction but the lifter didn't properly lift the taken block!");

                            block.AddRange(takenBlock.GetInstructions());
                            block.SetBranchNotTaken(takenBlock.GetBranchNotTaken());
                            block.SetBranchTaken(takenBlock.GetBranchTaken());

                            // Make sure the instruction at the current index gets run
                            i--;

                            break;
                        }

                        case OpaquePredicateSolver.OpaquePredKind.NotOpaque:
                        {
                            Logger.Warning(GetName(),
                                $"0x{instruction.IP:X} : {instruction.ToString()} : Condition is not opaque");
                            return false;
                        }

                        case OpaquePredicateSolver.OpaquePredKind.Error:
                        {
                            Logger.Error(GetName(), "Unknown instruction was in the instruction stream.");

                            foreach (var dumpInstruction in instructions)
                            {
                                Logger.Info(GetName(), $"0x{dumpInstruction.IP:X} : {dumpInstruction.ToString()}");
                            }

                            return false;
                        }

                        default:
                            throw new ArgumentOutOfRangeException();
                    }

                    break;
                }

                case Mnemonic.Jcxz:
                case Mnemonic.Jecxz:
                case Mnemonic.Jrcxz:
                {
                    throw new NotImplementedException($"Solving for {instruction.ToString()} is not supported yet!");
                }

                case Mnemonic.Ret:
                case Mnemonic.Retf:
                {
                    block.SetBranchNotTaken(null);
                    block.SetBranchTaken(null);
                    break;
                }

                case Mnemonic.Jmp:
                {
                    if (instruction.FlowControl == FlowControl.IndirectBranch)
                    {
                        block.SetBranchNotTaken(null);
                        block.SetBranchTaken(null);
                        break;
                    }

                    // Psuedo-Branch Coalesce
                    if (instruction.FlowControl == FlowControl.UnconditionalBranch)
                    {
                        block.RemoveAt(i);
                        i--;
                        break;
                    }

                    Logger.Error(GetName(), "Unknown Jmp traced in VM enter!");

                    foreach (var dumpInstruction in instructions)
                    {
                        Logger.Debug(GetName(), $"0x{dumpInstruction.IP:X} : {dumpInstruction.ToString()}");
                    }

                    return false;
                }

                default:
                {
                    _translate.TranslateInstruction(instruction, ref ExecContext);
                    break;
                }
            }
        }

        return true;
    }

    internal ulong? GetHandlerAddress(BasicBlock lastBlock)
    {
        var lastInstruction = lastBlock.GetInstructions().Last();

        switch (lastInstruction.FlowControl)
        {
            case FlowControl.IndirectBranch:
            {
                var exprOfNewBlock =
                    ExecContext.IcedRegisterToContextRegister(_ctx, lastInstruction.Op0Register).Item1;

                if (exprOfNewBlock is not BitVecNum addrOfNewBlock)
                    return null;

                return addrOfNewBlock.UInt64;
            }

            case FlowControl.Return:
            {
                var exprOfNewBlock = ExecContext.Stack.PopExpression().Simplify();

                if (lastInstruction.Op0Kind.IsImmediate())
                    ExecContext.Stack.StackPointer = (BitVecNum)_ctx.MkBVAdd(ExecContext.Stack.StackPointer,
                        _ctx.MkBV(lastInstruction.GetImmediate(0), 64)).Simplify();

                if (exprOfNewBlock is not BitVecNum addrOfNewBlock)
                    return null;

                return addrOfNewBlock.UInt64;
            }

            default:
                throw new ArgumentOutOfRangeException();
        }
    }
}