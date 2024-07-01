using Microsoft.Z3;

namespace DVM64.Extensions;

public static class Z3
{
    public static IntExpr PopCount(this Context ctx, BitVecExpr expr)
    {
        const ulong c1 = 0x_55555555_55555555ul;
        const ulong c2 = 0x_33333333_33333333ul;
        const ulong c3 = 0x_0F0F0F0F_0F0F0F0Ful;
        const ulong c4 = 0x_01010101_01010101ul;

        // var e1 = value - (value >> 1) & c1;
        var e1 = (BitVecExpr)ctx.MkBVSub(expr,
            ctx.MkBVAND(ctx.MkBVASHR(expr, ctx.MkBV(1, expr.SortSize)), ctx.MkBV(c1, expr.SortSize))).Simplify();
        // var e2 = (value & c2) + ((value >> 2) & c2);
        var e2 = (BitVecExpr)ctx.MkBVAdd(ctx.MkBVAND(e1, ctx.MkBV(c2, expr.SortSize)),
            ctx.MkBVAND(ctx.MkBVASHR(e1, ctx.MkBV(2, expr.SortSize)), ctx.MkBV(c2, expr.SortSize))).Simplify();
        // var e3 = (((value + (value >> 4)) & c3) * c4) >> 56; (Bit count - 0x8)
        var e3 = (BitVecExpr)ctx
            .MkBVASHR(
                ctx.MkBVMul(
                    ctx.MkBVAND(ctx.MkBVAdd(e2, ctx.MkBVASHR(e2, ctx.MkBV(4, expr.SortSize))),
                        ctx.MkBV(c3, expr.SortSize)), ctx.MkBV(c4, expr.SortSize)),
                ctx.MkBV(expr.SortSize - 0x8, expr.SortSize)).Simplify();

        return ctx.MkBV2Int(e3, false);
    }
}