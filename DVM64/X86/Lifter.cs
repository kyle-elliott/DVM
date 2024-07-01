using System.Reflection.PortableExecutable;
using Iced.Intel;

namespace DVM64.X86;

internal class Lifter
{
    public Lifter(PEReader pe, ulong imageBase)
    {
        Reader = new PeMemoryBlockCodeReader(pe, imageBase);
        _decoder = Decoder.Create(64, Reader);
        _knownBlocks = [];
    }

    public readonly PeMemoryBlockCodeReader Reader;
    private readonly Decoder _decoder;
    private readonly HashSet<BasicBlock> _knownBlocks;

    private Instruction? Disassemble(ulong address)
    {
        if (!Reader.VirtualAddressInRange(address))
            return null;

        lock (Reader)
        {
            Reader.Position = (int)(uint)address;
            _decoder.IP = address;
            _decoder.Decode(out var instruction);

            if (_decoder.LastError == DecoderError.InvalidInstruction)
                return null;

            if (_decoder.LastError != DecoderError.None)
            {
                Logger.Error("Lifter", $"Got {_decoder.LastError} while disassembling at 0x{address:X}");
            }

            return instruction;
        }
    }

    private static ulong GetNotTakenAddress(Instruction instruction)
    {
        return instruction.FlowControl switch
        {
            FlowControl.Call => instruction.NearBranchTarget,
            FlowControl.UnconditionalBranch => instruction.NearBranchTarget,
            _ => instruction.NextIP
        };
    }

    private static ulong GetTakenAddress(Instruction instruction)
    {
        return instruction.FlowControl switch
        {
            FlowControl.Call => instruction.NearBranchTarget,
            FlowControl.UnconditionalBranch => instruction.NearBranchTarget,
            FlowControl.ConditionalBranch => instruction.NearBranchTarget,
            _ => instruction.NextIP
        };
    }

    private static bool IsEndOfBlock(Instruction instruction)
    {
        return instruction.FlowControl switch
        {
            FlowControl.IndirectBranch => true,
            FlowControl.Return => true,
            _ => false
        };
    }

    internal BasicBlock? Lift(ulong address)
    {
        return Lift(null, address);
    }

    private BasicBlock? Lift(BasicBlock? parent, ulong address)
    {
        BasicBlock block = new(parent);
        var rip = address;

        while (true)
        {
            var instruction = Disassemble(rip);

            if (instruction == null)
                return null;

            block.Add(instruction.Value);

            if (IsEndOfBlock(instruction.Value))
                break;

            // Does the block take a conditional branch?
            var taken = GetTakenAddress(instruction.Value);
            var notTaken = GetNotTakenAddress(instruction.Value);

            if (taken != notTaken)
            {
                var takenBlock = FindKnownBlock(taken) ?? FindParent(block, taken) ?? Lift(block, taken);
                var notTakenBlock = FindKnownBlock(notTaken) ?? FindParent(block, notTaken) ?? Lift(block, notTaken);

                block.SetBranchTaken(takenBlock);
                block.SetBranchNotTaken(notTakenBlock);

                break;
            }

            rip = taken;
        }

        _knownBlocks.Add(block);
        return block;
    }

    private static BasicBlock? FindParent(BasicBlock currentBlock, ulong desiredBlock)
    {
        if (currentBlock.GetAddress() == desiredBlock)
            return currentBlock;

        var checkedBlock = currentBlock.GetParent();
        while (checkedBlock != null)
        {
            if (checkedBlock.GetAddress() == desiredBlock)
                return checkedBlock;

            checkedBlock = checkedBlock.GetParent();
        }

        return null;
    }

    private BasicBlock? FindKnownBlock(ulong desiredBlock)
    {
        return _knownBlocks.FirstOrDefault(x => x.GetAddress() == desiredBlock);
    }
}