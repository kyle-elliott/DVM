using Iced.Intel;

namespace DVM64.Extensions;

public static partial class Iced
{
    public static bool IsStackRegister(this Register register)
    {
        return register is Register.SP
                        or Register.SPL
                        or Register.ESP
                        or Register.RSP;
    }
}