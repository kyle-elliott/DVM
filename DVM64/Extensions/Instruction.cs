using Iced.Intel;

namespace DVM64.Extensions;

public static partial class Iced
{
    public static bool CompareOperands(this Instruction ins0, Instruction ins1, int ins0Index, int ins1Index)
    {
        var op0 = ins0.GetOpKind(ins0Index);
        var op1 = ins1.GetOpKind(ins1Index);

        if (op0 != op1)
            return false;

        if (op0.IsImmediate())
            return ins0.GetImmediate(ins0Index) == ins1.GetImmediate(ins1Index);

        if (op0 == OpKind.Memory)
            return ins0.MemoryBase == ins1.MemoryBase &&
                   ins0.MemoryIndex == ins1.MemoryIndex &&
                   ins0.MemoryIndexScale == ins1.MemoryIndexScale &&
                   ins0.MemoryDisplacement64 == ins1.MemoryDisplacement64;

        return op0 == op1;
    }

    public static dynamic GetOperand(this Instruction ins, int index)
    {
        if (ins.GetOpKind(index).IsImmediate())
            return ins.GetImmediate(index);

        if (ins.GetOpKind(index) == OpKind.Register)
            return ins.GetOpRegister(index);

        if (ins.GetOpKind(index) == OpKind.Memory)
            return new MemoryOperand(ins.MemoryBase, ins.MemoryIndex, ins.MemoryIndexScale, (long)ins.MemoryDisplacement64, ins.MemoryDisplSize);

        throw new ArgumentOutOfRangeException(nameof(index), ins.GetOpKind(index).ToString());
    }

    public static uint GetOperandSize(this Instruction ins, int index)
    {
        if (ins.GetOpKind(index).IsImmediate())
        {
            return ins.Op0Kind switch
            {
                OpKind.Immediate8 or OpKind.Immediate8_2nd => 1u,
                OpKind.Immediate8to16 or OpKind.Immediate16 => 2u,
                OpKind.Immediate8to32 or OpKind.Immediate32 => 4u,
                OpKind.Immediate8to64 or OpKind.Immediate32to64 or OpKind.Immediate64 => 8u,
                _ => throw new ArgumentOutOfRangeException(nameof(index), ins.GetOpKind(index).ToString())
            };
        }

        if (ins.GetOpKind(index) == OpKind.Register)
            return (uint)ins.GetOpRegister(index).GetSize();

        if (ins.GetOpKind(index) == OpKind.Memory)
        {
            return ins.MemorySize switch
            {
                MemorySize.Int8 or MemorySize.UInt8 => 1u,
                MemorySize.Int16 or MemorySize.UInt16 => 2u,
                MemorySize.Int32 or MemorySize.UInt32 => 4u,
                MemorySize.Int64 or MemorySize.UInt64 => 8u,
                _ => throw new ArgumentOutOfRangeException(nameof(index), $"{ins.MemorySize.ToString()}")
            };
        }

        throw new ArgumentOutOfRangeException(nameof(index), ins.GetOpKind(index).ToString());
    }
}