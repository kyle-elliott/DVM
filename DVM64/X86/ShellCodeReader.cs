using Iced.Intel;

namespace DVM64.X86;

/// <summary>
/// A <see cref="CodeReader"/> that reads data from a <see cref="System.Collections.Generic.List"/>
/// </summary>
public class ShellCodeReader(ulong baseAddress) : PositionCodeReader
{
    public ShellCodeReader(int bitness, ulong baseAddress, List<Instruction> shellcode) : this(baseAddress)
    {
        var asm = new Assembler(bitness);
        shellcode.ForEach(x => asm.AddInstruction(x));
        var stream = new MemoryStream();
        asm.Assemble(new StreamCodeWriter(stream), baseAddress);
        _byteCode = stream.ToArray();
    }
    
    public override int Position { get; set; }
    
    public override bool VirtualAddressInRange(ulong virtualAddress)
    {
        return virtualAddress >= baseAddress && virtualAddress <= (ulong)_byteCode.Length + baseAddress;
    }
    
    public override int ReadByte()
    {
        return _byteCode[Position++];
    }

    
    private Byte[] _byteCode;
}