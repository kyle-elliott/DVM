using DVM64.Extensions;
using Iced.Intel;
using Microsoft.Z3;

namespace DVM64_Tests.Helpers.BitVecExpr;

public class PopCountTest
{
    [Fact]
    public void TestCountBits_8Bit()
    {
        var ctx = new Context();
        var expr = ctx.MkBV(0xF1, 8);
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());
        var result = (IntNum)solver.Model.Eval( ctx.PopCount(expr), true);
        
        Assert.Equal(5, result.Int);
    }
    
    [Fact]
    public void TestCountBits_16Bit()
    {
        var ctx = new Context();
        var expr = ctx.MkBV(0x1234, 16);
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());
        var result = (IntNum)solver.Model.Eval( ctx.PopCount(expr), true);
        
        Assert.Equal(5, result.Int);
    }
    
    [Fact]
    public void TestCountBits_32Bit()
    {
        var ctx = new Context();
        var expr = ctx.MkBV(0x12345678, 32);
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());
        var result = (IntNum)solver.Model.Eval( ctx.PopCount(expr), true);
        
        Assert.Equal(13, result.Int);
    }
    
    [Fact]
    public void TestCountBits_32Bit_Neg()
    {
        var ctx = new Context();
        var expr = ctx.MkBV(-420, 32);
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());
        var result = (IntNum)solver.Model.Eval( ctx.PopCount(expr), true);
        
        Assert.Equal(27, result.Int);
    }
    
    [Fact]
    public void TestCountBits_64Bit()
    {
        var ctx = new Context();
        var expr = ctx.MkBV(0xDEADBEEF, 64);
        
        var solver = ctx.MkSolver();
        Assert.Equal(Status.SATISFIABLE, solver.Check());
        var result = (IntNum)solver.Model.Eval( ctx.PopCount(expr), true);
        
        Assert.Equal(24, result.Int);
    }
}