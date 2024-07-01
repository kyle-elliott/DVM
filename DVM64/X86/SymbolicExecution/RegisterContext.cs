using DVM64.Extensions;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64.X86.SymbolicExecution;

/// <summary>
/// Defines the kinds of contexts that can be represented, including various register sizes and types.
/// </summary>
public enum ContextKind
{
    Register64,
    RegisterH32,
    RegisterL32,
    RegisterH16,
    RegisterL16,
    RegisterH8,
    RegisterL8,
    Memory,
    Immediate
}

/// <summary>
/// Represents the context of registers for x86 architecture, including flags and stack state.
/// </summary>
public class RegisterContext
{
    // Register expressions for general-purpose registers.
    public BitVecExpr Rax;
    public BitVecExpr Rbx;
    public BitVecExpr Rcx;
    public BitVecExpr Rdx;

    public BitVecExpr Rbp;

    // Rsp is not present within here because it's stored in ExpressionStack
    public BitVecExpr Rsi;
    public BitVecExpr Rdi;
    public BitVecExpr R8;
    public BitVecExpr R9;
    public BitVecExpr R10;
    public BitVecExpr R11;
    public BitVecExpr R12;
    public BitVecExpr R13;
    public BitVecExpr R14;
    public BitVecExpr R15;

    // Register expression for instruction pointer.
    public BitVecExpr Rip;

    // Register expression for flags register.
    public BitVecExpr RFlags;

    public BitVecExpr CR0;
    public BitVecExpr CR1;
    public BitVecExpr CR2;
    public BitVecExpr CR3;
    public BitVecExpr CR4;
    public BitVecExpr CR5;
    public BitVecExpr CR6;
    public BitVecExpr CR7;
    public BitVecExpr CR8;
    public BitVecExpr CR9;
    public BitVecExpr CR10;
    public BitVecExpr CR11;
    public BitVecExpr CR12;
    public BitVecExpr CR13;
    public BitVecExpr CR14;
    public BitVecExpr CR15;

    // Boolean expressions for individual flags.
    public BoolExpr Cf; // bit 0
    public BoolExpr Pf; // bit 2
    public BoolExpr Af; // bit 4
    public BoolExpr Zf; // bit 6
    public BoolExpr Sf; // bit 7
    public BoolExpr Tf; // bit 8
    public BoolExpr If; // bit 9
    public BoolExpr Df; // bit 10
    public BoolExpr Of; // bit 11

    // Stack state represented as a list of expressions.
    public readonly ExpressionStack Stack;

    /// <summary>
    /// Initializes a new instance of the <see cref="RegisterContext"/> class with initial values using a Z3 context.
    /// This constructor is used when creating a context with a specific initial state, defined within a Z3 context.
    /// </summary>
    /// <param name="ctx">The Z3 context used for creating initial register and flag expressions.</param>
    /// <param name="stackPointer">The initial value for the internal stack pointer.</param>
    /// <param name="rip">The initial value for the internal instruction pointer.</param>
    public RegisterContext(Context ctx, BitVecNum stackPointer, BitVecNum rip)
    {
        Rax = ctx.MkBVConst("init_rax", 64);
        Rbx = ctx.MkBVConst("init_rbx", 64);
        Rcx = ctx.MkBVConst("init_rcx", 64);
        Rdx = ctx.MkBVConst("init_rdx", 64);
        Rbp = ctx.MkBVConst("init_rbp", 64);

        Rsi = ctx.MkBVConst("init_rsi", 64);
        Rdi = ctx.MkBVConst("init_rdi", 64);
        R8 = ctx.MkBVConst("init_r8", 64);
        R9 = ctx.MkBVConst("init_r9", 64);
        R10 = ctx.MkBVConst("init_r10", 64);
        R11 = ctx.MkBVConst("init_r11", 64);
        R12 = ctx.MkBVConst("init_r12", 64);
        R13 = ctx.MkBVConst("init_r13", 64);
        R14 = ctx.MkBVConst("init_r14", 64);
        R15 = ctx.MkBVConst("init_r15", 64);
        Rip = rip;
        RFlags = ctx.MkBVConst("init_rflags", 64);

        CR0 = ctx.MkBVConst("init_cr0", 64);
        CR1 = ctx.MkBVConst("init_cr1", 64);
        CR2 = ctx.MkBVConst("init_cr2", 64);
        CR3 = ctx.MkBVConst("init_cr3", 64);
        CR4 = ctx.MkBVConst("init_cr4", 64);
        CR5 = ctx.MkBVConst("init_cr5", 64);
        CR6 = ctx.MkBVConst("init_cr6", 64);
        CR7 = ctx.MkBVConst("init_cr7", 64);
        CR8 = ctx.MkBVConst("init_cr8", 64);
        CR9 = ctx.MkBVConst("init_cr9", 64);
        CR10 = ctx.MkBVConst("init_cr10", 64);
        CR11 = ctx.MkBVConst("init_cr11", 64);
        CR12 = ctx.MkBVConst("init_cr12", 64);
        CR13 = ctx.MkBVConst("init_cr13", 64);
        CR14 = ctx.MkBVConst("init_cr14", 64);
        CR15 = ctx.MkBVConst("init_cr15", 64);

        Cf = ctx.MkBoolConst("init_cf");
        Pf = ctx.MkBoolConst("init_pf");
        Af = ctx.MkBoolConst("init_af");
        Zf = ctx.MkBoolConst("init_zf");
        Sf = ctx.MkBoolConst("init_sf");
        Tf = ctx.MkBoolConst("init_tf");
        If = ctx.MkBoolConst("init_if");
        Df = ctx.MkBoolConst("init_df");
        Of = ctx.MkBoolConst("init_of");

        Stack = new ExpressionStack(ctx, stackPointer);
    }

    /// <summary>
    /// Simplifies all register and flag expressions using Z3's Simplify method.
    /// This method is used to reduce the complexity of expressions, potentially making further analysis more efficient.
    /// </summary>
    public void Simplify()
    {
        Rax = (BitVecExpr)Rax.Simplify();
        Rbx = (BitVecExpr)Rbx.Simplify();
        Rcx = (BitVecExpr)Rcx.Simplify();
        Rdx = (BitVecExpr)Rdx.Simplify();
        Rbp = (BitVecExpr)Rbp.Simplify();
        Rsi = (BitVecExpr)Rsi.Simplify();
        Rdi = (BitVecExpr)Rdi.Simplify();
        R8 = (BitVecExpr)R8.Simplify();
        R9 = (BitVecExpr)R9.Simplify();
        R10 = (BitVecExpr)R10.Simplify();
        R11 = (BitVecExpr)R11.Simplify();
        R12 = (BitVecExpr)R12.Simplify();
        R13 = (BitVecExpr)R13.Simplify();
        R14 = (BitVecExpr)R14.Simplify();
        R15 = (BitVecExpr)R15.Simplify();

        Rip = (BitVecExpr)Rip.Simplify();
        RFlags = (BitVecExpr)RFlags.Simplify();

        CR0 = (BitVecExpr)CR0.Simplify();
        CR1 = (BitVecExpr)CR1.Simplify();
        CR2 = (BitVecExpr)CR2.Simplify();
        CR3 = (BitVecExpr)CR3.Simplify();
        CR4 = (BitVecExpr)CR4.Simplify();
        CR5 = (BitVecExpr)CR5.Simplify();
        CR6 = (BitVecExpr)CR6.Simplify();
        CR7 = (BitVecExpr)CR7.Simplify();
        CR8 = (BitVecExpr)CR8.Simplify();
        CR9 = (BitVecExpr)CR9.Simplify();
        CR10 = (BitVecExpr)CR10.Simplify();
        CR11 = (BitVecExpr)CR11.Simplify();
        CR12 = (BitVecExpr)CR12.Simplify();
        CR13 = (BitVecExpr)CR13.Simplify();
        CR14 = (BitVecExpr)CR14.Simplify();
        CR15 = (BitVecExpr)CR15.Simplify();

        Cf = (BoolExpr)Cf.Simplify();
        Pf = (BoolExpr)Pf.Simplify();
        Af = (BoolExpr)Af.Simplify();
        Zf = (BoolExpr)Zf.Simplify();
        Sf = (BoolExpr)Sf.Simplify();
        Tf = (BoolExpr)Tf.Simplify();
        If = (BoolExpr)If.Simplify();
        Df = (BoolExpr)Df.Simplify();
        Of = (BoolExpr)Of.Simplify();

        Stack.Simplify();
        Stack.Align();
    }

    /// <summary>
    /// Retrieves a sub-register expression based on the specified kind.
    /// This method is used to extract a specific portion of a register, such as the lower 8 bits of a 32-bit register.
    /// </summary>
    /// <param name="ctx">The Z3 context used for expression manipulation.</param>
    /// <param name="register">The full register expression.</param>
    /// <param name="kind">The kind of sub-register to retrieve.</param>
    /// <returns>A simplified bit vector expression representing the sub-register.</returns>
    private static (BitVecExpr, ContextKind) GetSubRegisterExpression(Context ctx, BitVecExpr register,
        ContextKind kind)
    {
        return (kind switch
        {
            ContextKind.RegisterH32 => (BitVecExpr)ctx.MkExtract(63, 32, register).Simplify(),
            ContextKind.RegisterL32 => (BitVecExpr)ctx.MkExtract(31, 0, register).Simplify(),
            ContextKind.RegisterH16 => (BitVecExpr)ctx.MkExtract(31, 16, register).Simplify(),
            ContextKind.RegisterL16 => (BitVecExpr)ctx.MkExtract(15, 0, register).Simplify(),
            ContextKind.RegisterH8 => (BitVecExpr)ctx.MkExtract(15, 8, register).Simplify(),
            ContextKind.RegisterL8 => (BitVecExpr)ctx.MkExtract(7, 0, register).Simplify(),
            _ => throw new ArgumentOutOfRangeException(nameof(kind), kind,
                $"GetSubRegisterExpression does not support {kind}!")
        }, kind);
    }

    /// <summary>
    /// Converts an Iced Register to a context-specific register representation.
    /// This method maps an Iced Register enumeration value to the corresponding register expression and context kind within this context.
    /// </summary>
    /// <param name="ctx">The Z3 context used for expression manipulation.</param>
    /// <param name="register">The Iced Register to convert.</param>
    /// <returns>A tuple containing the register expression and its context kind.</returns>
    public (BitVecExpr, ContextKind) IcedRegisterToContextRegister(Context ctx, Register register)
    {
        switch (register)
        {
            case Register.RAX:
                return (Rax, ContextKind.Register64);

            case Register.EAX:
                return GetSubRegisterExpression(ctx, Rax, ContextKind.RegisterL32);

            case Register.AX:
                return GetSubRegisterExpression(ctx, Rax, ContextKind.RegisterL16);

            case Register.AH:
                return GetSubRegisterExpression(ctx, Rax, ContextKind.RegisterH8);

            case Register.AL:
                return GetSubRegisterExpression(ctx, Rax, ContextKind.RegisterL8);

            case Register.RBX:
                return (Rbx, ContextKind.Register64);

            case Register.EBX:
                return GetSubRegisterExpression(ctx, Rbx, ContextKind.RegisterL32);

            case Register.BX:
                return GetSubRegisterExpression(ctx, Rbx, ContextKind.RegisterL16);

            case Register.BH:
                return GetSubRegisterExpression(ctx, Rbx, ContextKind.RegisterH8);

            case Register.BL:
                return GetSubRegisterExpression(ctx, Rbx, ContextKind.RegisterL8);

            case Register.RCX:
                return (Rcx, ContextKind.Register64);

            case Register.ECX:
                return GetSubRegisterExpression(ctx, Rcx, ContextKind.RegisterL32);

            case Register.CX:
                return GetSubRegisterExpression(ctx, Rcx, ContextKind.RegisterL16);

            case Register.CH:
                return GetSubRegisterExpression(ctx, Rcx, ContextKind.RegisterH8);

            case Register.CL:
                return GetSubRegisterExpression(ctx, Rcx, ContextKind.RegisterL8);

            case Register.RDX:
                return (Rdx, ContextKind.Register64);

            case Register.EDX:
                return GetSubRegisterExpression(ctx, Rdx, ContextKind.RegisterL32);

            case Register.DX:
                return GetSubRegisterExpression(ctx, Rdx, ContextKind.RegisterL16);

            case Register.DH:
                return GetSubRegisterExpression(ctx, Rdx, ContextKind.RegisterH8);

            case Register.DL:
                return GetSubRegisterExpression(ctx, Rdx, ContextKind.RegisterL8);

            case Register.RBP:
                return (Rbp, ContextKind.Register64);

            case Register.EBP:
                return GetSubRegisterExpression(ctx, Rbp, ContextKind.RegisterL32);

            case Register.BP:
                return GetSubRegisterExpression(ctx, Rbp, ContextKind.RegisterL16);

            case Register.BPL:
                return GetSubRegisterExpression(ctx, Rbp, ContextKind.RegisterL8);

            case Register.RSP:
                return (Stack.StackPointer, ContextKind.Register64);

            case Register.ESP:
                return GetSubRegisterExpression(ctx, Stack.StackPointer, ContextKind.RegisterL32);

            case Register.SP:
                return GetSubRegisterExpression(ctx, Stack.StackPointer, ContextKind.RegisterL16);

            case Register.SPL:
                return GetSubRegisterExpression(ctx, Stack.StackPointer, ContextKind.RegisterL8);

            case Register.RSI:
                return (Rsi, ContextKind.Register64);

            case Register.ESI:
                return GetSubRegisterExpression(ctx, Rsi, ContextKind.RegisterL32);

            case Register.SI:
                return GetSubRegisterExpression(ctx, Rsi, ContextKind.RegisterL16);

            case Register.SIL:
                return GetSubRegisterExpression(ctx, Rsi, ContextKind.RegisterL8);

            case Register.RDI:
                return (Rdi, ContextKind.Register64);

            case Register.EDI:
                return GetSubRegisterExpression(ctx, Rdi, ContextKind.RegisterL32);

            case Register.DI:
                return GetSubRegisterExpression(ctx, Rdi, ContextKind.RegisterL16);

            case Register.DIL:
                return GetSubRegisterExpression(ctx, Rdi, ContextKind.RegisterL8);

            case Register.R8:
                return (R8, ContextKind.Register64);

            case Register.R8D:
                return GetSubRegisterExpression(ctx, R8, ContextKind.RegisterL32);

            case Register.R8W:
                return GetSubRegisterExpression(ctx, R8, ContextKind.RegisterL16);

            case Register.R8L:
                return GetSubRegisterExpression(ctx, R8, ContextKind.RegisterL8);

            case Register.R9:
                return (R9, ContextKind.Register64);

            case Register.R9D:
                return GetSubRegisterExpression(ctx, R9, ContextKind.RegisterL32);

            case Register.R9W:
                return GetSubRegisterExpression(ctx, R9, ContextKind.RegisterL16);

            case Register.R9L:
                return GetSubRegisterExpression(ctx, R9, ContextKind.RegisterL8);

            case Register.R10:
                return (R10, ContextKind.Register64);

            case Register.R10D:
                return GetSubRegisterExpression(ctx, R10, ContextKind.RegisterL32);

            case Register.R10W:
                return GetSubRegisterExpression(ctx, R10, ContextKind.RegisterL16);

            case Register.R10L:
                return GetSubRegisterExpression(ctx, R10, ContextKind.RegisterL8);

            case Register.R11:
                return (R11, ContextKind.Register64);

            case Register.R11D:
                return GetSubRegisterExpression(ctx, R11, ContextKind.RegisterL32);

            case Register.R11W:
                return GetSubRegisterExpression(ctx, R11, ContextKind.RegisterL16);

            case Register.R11L:
                return GetSubRegisterExpression(ctx, R11, ContextKind.RegisterL8);

            case Register.R12:
                return (R12, ContextKind.Register64);

            case Register.R12D:
                return GetSubRegisterExpression(ctx, R12, ContextKind.RegisterL32);

            case Register.R12W:
                return GetSubRegisterExpression(ctx, R12, ContextKind.RegisterL16);

            case Register.R12L:
                return GetSubRegisterExpression(ctx, R12, ContextKind.RegisterL8);

            case Register.R13:
                return (R13, ContextKind.Register64);

            case Register.R13D:
                return GetSubRegisterExpression(ctx, R13, ContextKind.RegisterL32);

            case Register.R13W:
                return GetSubRegisterExpression(ctx, R13, ContextKind.RegisterL16);

            case Register.R13L:
                return GetSubRegisterExpression(ctx, R13, ContextKind.RegisterL8);

            case Register.R14:
                return (R14, ContextKind.Register64);

            case Register.R14D:
                return GetSubRegisterExpression(ctx, R14, ContextKind.RegisterL32);

            case Register.R14W:
                return GetSubRegisterExpression(ctx, R14, ContextKind.RegisterL16);

            case Register.R14L:
                return GetSubRegisterExpression(ctx, R14, ContextKind.RegisterL8);

            case Register.R15:
                return (R15, ContextKind.Register64);

            case Register.R15D:
                return GetSubRegisterExpression(ctx, R15, ContextKind.RegisterL32);

            case Register.R15W:
                return GetSubRegisterExpression(ctx, R15, ContextKind.RegisterL16);

            case Register.R15L:
                return GetSubRegisterExpression(ctx, R15, ContextKind.RegisterL8);

            case Register.RIP:
                return (Rip, ContextKind.Register64);

            case Register.EIP:
                return GetSubRegisterExpression(ctx, Rip, ContextKind.RegisterL32);

            case Register.CR0:
                return (CR0, ContextKind.Register64);

            case Register.CR1:
                return (CR1, ContextKind.Register64);

            case Register.CR2:
                return (CR2, ContextKind.Register64);

            case Register.CR3:
                return (CR3, ContextKind.Register64);

            case Register.CR4:
                return (CR4, ContextKind.Register64);

            case Register.CR5:
                return (CR5, ContextKind.Register64);

            case Register.CR6:
                return (CR6, ContextKind.Register64);

            case Register.CR7:
                return (CR7, ContextKind.Register64);

            case Register.CR8:
                return (CR8, ContextKind.Register64);

            case Register.CR9:
                return (CR9, ContextKind.Register64);

            case Register.CR10:
                return (CR10, ContextKind.Register64);

            case Register.CR11:
                return (CR11, ContextKind.Register64);

            case Register.CR12:
                return (CR12, ContextKind.Register64);

            case Register.CR13:
                return (CR13, ContextKind.Register64);

            case Register.CR14:
                return (CR14, ContextKind.Register64);

            case Register.CR15:
                return (CR15, ContextKind.Register64);

            // TODO: Handle flags?
            default:
                throw new ArgumentOutOfRangeException(nameof(register), register, null);
        }
    }

    private static BitVecExpr HandleStoreSubRegister(Context ctx, BitVecExpr regExpr, BitVecExpr expr, ContextKind kind)
    {
        switch (kind)
        {
            case ContextKind.Register64:
                return expr;

            case ContextKind.RegisterL32:
            {
                var (l32, _) = GetSubRegisterExpression(ctx, expr, ContextKind.RegisterL32);
                return (BitVecExpr)ctx.MkZeroExt(32, l32).Simplify();
            }

            case ContextKind.RegisterL16:
            {
                var (h32, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterH32);
                var (h16, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterH16);
                var (l16, _) = GetSubRegisterExpression(ctx, expr, ContextKind.RegisterL16);
                return (BitVecExpr)ctx.MkConcat(h32, ctx.MkConcat(h16, l16)).Simplify();
            }

            case ContextKind.RegisterH8:
            {
                var (h32, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterH32);
                var (h16, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterH16);
                var (h8, _) = GetSubRegisterExpression(ctx, expr, ContextKind.RegisterL8);
                var (l8, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterL8);
                return (BitVecExpr)ctx.MkConcat(h32, ctx.MkConcat(h16, ctx.MkConcat(h8, l8))).Simplify();
            }

            case ContextKind.RegisterL8:
            {
                var (h32, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterH32);
                var (h16, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterH16);
                var (h8, _) = GetSubRegisterExpression(ctx, regExpr, ContextKind.RegisterH8);
                var (l8, _) = GetSubRegisterExpression(ctx, expr, ContextKind.RegisterL8);
                return (BitVecExpr)ctx.MkConcat(h32, ctx.MkConcat(h16, ctx.MkConcat(h8, l8))).Simplify();
            }

            default:
                throw new ArgumentOutOfRangeException(nameof(kind), kind, null);
        }
    }

    /// <summary>
    /// Stores an expression in a context-specific register based on the specified kind.
    /// This method is used to update the state of a register within the context, taking into account the specific kind of the register.
    /// </summary>
    /// <param name="ctx">The Z3 context used for expression manipulation.</param>
    /// <param name="expr">The expression to store.</param>
    /// <param name="register">The target register.</param>
    /// <param name="kind">The kind of the target register.</param>
    public void StoreExprInContextRegister(Context ctx, Expr expr, Register register, ContextKind kind)
    {
        switch (register)
        {
            case Register.RAX:
            case Register.EAX:
            case Register.AX:
            case Register.AH:
            case Register.AL:
            {
                Rax = HandleStoreSubRegister(ctx, Rax, (BitVecExpr)expr, kind);
                return;
            }

            case Register.RBX:
            case Register.EBX:
            case Register.BX:
            case Register.BH:
            case Register.BL:
            {
                Rbx = HandleStoreSubRegister(ctx, Rbx, (BitVecExpr)expr, kind);
                return;
            }

            case Register.RCX:
            case Register.ECX:
            case Register.CX:
            case Register.CH:
            case Register.CL:
            {
                Rcx = HandleStoreSubRegister(ctx, Rcx, (BitVecExpr)expr, kind);
                return;
            }

            case Register.RDX:
            case Register.EDX:
            case Register.DX:
            case Register.DH:
            case Register.DL:
            {
                Rdx = HandleStoreSubRegister(ctx, Rdx, (BitVecExpr)expr, kind);
                return;
            }

            case Register.RBP:
            case Register.EBP:
            case Register.BP:
            case Register.BPL:
            {
                Rbp = HandleStoreSubRegister(ctx, Rbp, (BitVecExpr)expr, kind);
                return;
            }

            case Register.RSP:
            case Register.ESP:
            case Register.SP:
            case Register.SPL:
            {
                Stack.StackPointer = (BitVecNum)HandleStoreSubRegister(ctx, Stack.StackPointer, (BitVecExpr)expr, kind);
                return;
            }

            case Register.RSI:
            case Register.ESI:
            case Register.SI:
            case Register.SIL:
            {
                Rsi = HandleStoreSubRegister(ctx, Rsi, (BitVecExpr)expr, kind);
                return;
            }

            case Register.RDI:
            case Register.EDI:
            case Register.DI:
            case Register.DIL:
            {
                Rdi = HandleStoreSubRegister(ctx, Rdi, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R8:
            case Register.R8D:
            case Register.R8W:
            case Register.R8L:
            {
                R8 = HandleStoreSubRegister(ctx, R8, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R9:
            case Register.R9D:
            case Register.R9W:
            case Register.R9L:
            {
                R9 = HandleStoreSubRegister(ctx, R9, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R10:
            case Register.R10D:
            case Register.R10W:
            case Register.R10L:
            {
                R10 = HandleStoreSubRegister(ctx, R10, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R11:
            case Register.R11D:
            case Register.R11W:
            case Register.R11L:
            {
                R11 = HandleStoreSubRegister(ctx, R11, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R12:
            case Register.R12D:
            case Register.R12W:
            case Register.R12L:
            {
                R12 = HandleStoreSubRegister(ctx, R12, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R13:
            case Register.R13D:
            case Register.R13W:
            case Register.R13L:
            {
                R13 = HandleStoreSubRegister(ctx, R13, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R14:
            case Register.R14D:
            case Register.R14W:
            case Register.R14L:
            {
                R14 = HandleStoreSubRegister(ctx, R14, (BitVecExpr)expr, kind);
                return;
            }

            case Register.R15:
            case Register.R15D:
            case Register.R15W:
            case Register.R15L:
            {
                R15 = HandleStoreSubRegister(ctx, R15, (BitVecExpr)expr, kind);
                return;
            }

            default:
                throw new ArgumentOutOfRangeException(nameof(register), register, "Unable to concretize to register!");
        }
    }

    /// <summary>
    /// Calculates common flags (ZF, SF, PF) based on the result of an operation.
    /// This method updates the Zero Flag (ZF), Sign Flag (SF), and Parity Flag (PF) based on the provided result expression.
    /// </summary>
    /// <param name="ctx">The Z3 context used for expression manipulation.</param>
    /// <param name="res">The result expression of an operation.</param>
    public void CalculateCommonFlags(Context ctx, BitVecExpr res)
    {
        // If the value is equal to 0
        Zf = (BoolExpr)ctx.MkEq(res, ctx.MkBV(0, res.SortSize)).Simplify();

        // If the MSB is set, the number is negative and this becomes 1
        Sf = (BoolExpr)ctx.MkEq(ctx.MkExtract(res.SortSize - 1, res.SortSize - 1, res),
            ctx.MkBV(1, 1)).Simplify();

        // Isolate the least significant byte and check if the number of bits that are set is even
        Pf = (BoolExpr)ctx.MkEq(ctx.MkMod(ctx.PopCount(ctx.MkExtract(7, 0, res)), ctx.MkInt(2)),
            ctx.MkInt(0)).Simplify();
    }

    public void CalculateAuxFlag(Context ctx, BitVecExpr e1, BitVecExpr e2, BitVecExpr res)
    {
        // Isolate bits 3 and 4 and xor them, then check if there's a carry into bit 5
        Af = (BoolExpr)ctx
            .MkEq(ctx.MkBVAND(ctx.MkBVXOR(ctx.MkBVXOR(e1, e2), res), ctx.MkBV(0x10, res.SortSize)),
                ctx.MkBV(0x10, res.SortSize)).Simplify();
    }

    public void CalculateAddFlags(Context ctx, BitVecExpr e1, BitVecExpr e2, BitVecExpr res)
    {
        Cf = (BoolExpr)ctx.MkBVULT(ctx.MkBVAdd(e1, e2), e1).Simplify();
        Of = (BoolExpr)ctx.MkBVSLT(ctx.MkBVAND(ctx.MkBVXOR(e1, res), ctx.MkBVXOR(e2, res)), ctx.MkBV(0, res.SortSize))
            .Simplify();
    }

    public void CalculateSubFlags(Context ctx, BitVecExpr e1, BitVecExpr e2, BitVecExpr res)
    {
        Cf = (BoolExpr)ctx.MkBVUGT(ctx.MkBVSub(e1, e2), e1).Simplify();
        Of = (BoolExpr)ctx.MkBVSLT(ctx.MkBVAND(ctx.MkBVXOR(e1, e2), ctx.MkBVXOR(e1, res)), ctx.MkBV(0, res.SortSize))
            .Simplify();
    }

    public override string ToString()
    {
        return $"RAX: {Rax}\n" +
               $"RBX: {Rbx}\n" +
               $"RCX: {Rcx}\n" +
               $"RDX: {Rdx}\n" +
               $"RBP: {Rbp}\n" +
               $"RSI: {Rsi}\n" +
               $"RDI: {Rdi}\n" +
               $"R8: {R8}\n" +
               $"R9: {R9}\n" +
               $"R10: {R10}\n" +
               $"R11: {R11}\n" +
               $"R12: {R12}\n" +
               $"R13: {R13}\n" +
               $"R14: {R14}\n" +
               $"R15: {R15}\n" +
               $"CR0: {CR0}\n" +
               $"CR1: {CR1}\n" +
               $"CR2: {CR2}\n" +
               $"CR3: {CR3}\n" +
               $"CR4: {CR4}\n" +
               $"CR5: {CR5}\n" +
               $"CR6: {CR6}\n" +
               $"CR7: {CR7}\n" +
               $"CR8: {CR8}\n" +
               $"CR9: {CR9}\n" +
               $"CR10: {CR10}\n" +
               $"CR11: {CR11}\n" +
               $"CR12: {CR12}\n" +
               $"CR13: {CR13}\n" +
               $"CR14: {CR14}\n" +
               $"CR15: {CR15}\n" +
               $"CF: {Cf}\n" +
               $"PF: {Pf}\n" +
               $"AF: {Af}\n" +
               $"ZF: {Zf}\n" +
               $"SF: {Sf}\n" +
               $"TF: {Tf}\n" +
               $"IF: {If}\n" +
               $"DF: {Df}\n" +
               $"OF: {Of}\n" +
               $"Stack: {Stack}";
    }
}