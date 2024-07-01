using Iced.Intel;

namespace DVM64.Extensions;

public static partial class Iced
{
    public static bool IsImmediate(this OpKind kind)
    {
        return kind is OpKind.Immediate8
                    or OpKind.Immediate8_2nd
                    or OpKind.Immediate16
                    or OpKind.Immediate32
                    or OpKind.Immediate64
                    or OpKind.Immediate8to16
                    or OpKind.Immediate8to32
                    or OpKind.Immediate8to64
                    or OpKind.Immediate32to64;
    }
}