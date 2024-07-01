using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using Iced.Intel;

namespace DVM64.X86;

public abstract class PositionCodeReader : CodeReader
{
    public abstract int Position { get; set; }

    public abstract bool VirtualAddressInRange(ulong virtualAddress);
}

/// <summary>
/// A <see cref="CodeReader"/> that reads data from a <see cref="PEMemoryBlock"/>
/// </summary>
public class PeMemoryBlockCodeReader : PositionCodeReader
{
    public PeMemoryBlockCodeReader(PEReader pe, ulong imageBase)
    {
        _sectionAddrAndSize = pe.PEHeaders.SectionHeaders
            .Select(x => new Tuple<int, int, int>(x.VirtualAddress, x.VirtualSize, x.PointerToRawData)).ToList();
        _reader = pe.GetEntireImage().GetReader();
        _imageBase = imageBase;
    }

    private BlobReader _reader;
    private readonly ulong _imageBase;
    private readonly List<Tuple<int, int, int>> _sectionAddrAndSize;

    public override int Position
    {
        get => _reader.Offset;
        set => _reader.Offset = VirtualToRawOffset(value - (int)(uint)_imageBase);
    }

    private int VirtualToRawOffset(int virtualAddress)
    {
        foreach (var (virtualAddr, virtualSize, pointerToRawData) in _sectionAddrAndSize)
        {
            if (virtualAddr <= virtualAddress && virtualAddress < virtualAddr + virtualSize)
                return pointerToRawData + (virtualAddress - virtualAddr);
        }

        throw new InvalidOperationException("Virtual address not found.");
    }

    public override bool VirtualAddressInRange(ulong virtualAddress)
    {
        foreach (var (virtualAddr, virtualSize, _) in _sectionAddrAndSize)
        {
            if ((ulong)virtualAddr + _imageBase <= virtualAddress &&
                virtualAddress < (ulong)virtualAddr + _imageBase + (ulong)virtualSize)
                return true;
        }

        return false;
    }

    public override int ReadByte()
    {
        return _reader.ReadByte();
    }
}