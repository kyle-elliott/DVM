using Iced.Intel;

namespace DVM64.X86.Passes;

/// <summary>
/// The BranchCoalesce class implements the IBlockPass interface and provides methods to optimize branch instructions.
/// </summary>
internal class BranchCoalesce : IBlockPass
{
    /// <summary>
    /// Returns the name of the pass.
    /// </summary>
    /// <returns>A string representing the name of the pass.</returns>
    public string GetName() => "BranchCoalesce";

    /// <summary>
    /// Runs the pass on a given basic block.
    /// </summary>
    /// <param name="block">The basic block to run the pass on.</param>
    /// <returns>A boolean indicating whether the pass was successful.</returns>
    public bool Run(BasicBlock block)
    {
        var instructions = block.GetInstructions();

        for (var i = 0; i < instructions.Count; i++)
        {
            var instruction = instructions[i];

            switch (instruction.FlowControl)
            {
                case FlowControl.UnconditionalBranch:
                    // Get rid of the branch, it's redundant
                    // This optimizes the code by removing unnecessary unconditional branches.
                    block.RemoveAt(i);
                    break;

                case FlowControl.ConditionalBranch:
                {
                    // Conditional has the same branch whether it's taken or not
                    if (instruction.NextIP == instruction.NearBranchTarget)
                    {
                        block.RemoveAt(i);
                    }

                    break;
                }
            }
        }

        return true;
    }
}