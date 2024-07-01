namespace DVM64.X86;

internal interface IBlockPass
{
    string GetName();
    bool Run(BasicBlock block);
}