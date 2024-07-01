using Iced.Intel;

namespace DVM64.X86;

internal class BasicBlock
{
    private BasicBlock? _branchTaken;
    private BasicBlock? _branchNotTaken;
    private readonly List<Instruction> _instructions = [];
    private readonly BasicBlock? _parent;

    internal BasicBlock(BasicBlock? parent)
    {
        _branchNotTaken = null;
        _branchTaken = null;
        _parent = parent;
    }

    internal void Add(Instruction instruction)
    {
        _instructions.Add(instruction);
    }

    internal void AddRange(IEnumerable<Instruction> instructions)
    {
        _instructions.AddRange(instructions);
    }

    internal void AddAfter(Instruction instruction, Instruction after)
    {
        var index = _instructions.IndexOf(after);
        _instructions.Insert(index + 1, instruction);
    }

    internal void AddBefore(Instruction instruction, Instruction before)
    {
        var index = _instructions.IndexOf(before);
        _instructions.Insert(index, instruction);
    }

    internal ulong GetAddress()
    {
        return _instructions[0].IP;
    }

    internal List<Instruction> GetInstructions()
    {
        return _instructions;
    }

    internal void Remove(ulong address)
    {
        var instruction = _instructions.First(instruction => instruction.IP == address);
        _instructions.Remove(instruction);
    }

    internal void RemoveAt(int index)
    {
        _instructions.RemoveAt(index);
    }

    internal void SetBranchTaken(BasicBlock? branch)
    {
        _branchTaken = branch;
    }

    internal void SetBranchNotTaken(BasicBlock? branch)
    {
        _branchNotTaken = branch;
    }

    internal BasicBlock? GetBranchTaken()
    {
        return _branchTaken;
    }

    internal BasicBlock? GetBranchNotTaken()
    {
        return _branchNotTaken;
    }

    internal BasicBlock? GetParent()
    {
        return _parent;
    }

    public override int GetHashCode()
    {
        // Get the hash code of all the instructions in _instructions, generate a hash of all combined, then return
        // that hash code
        return _instructions.Aggregate(0, (acc, instruction) => acc ^ instruction.GetHashCode());
    }
}