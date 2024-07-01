namespace DVM64.X86;

internal class BlockPassManager
{
    internal struct Stage(string name, IBlockPass[] passes)
    {
        public string Name { get; } = name;

        public List<IBlockPass> Passes { get; } = passes.ToList();
        public readonly List<int> BlockList = [];
    }
        
    internal void Register(IEnumerable<Stage> stage)
    {
            _stages.AddRange(stage);
        }

    private static bool RunStage(BasicBlock block, Stage stage)
    {
            if (stage.BlockList.Contains(block.GetHashCode()))
                return true;

            stage.BlockList.Add(block.GetHashCode());
            
            var taken = block.GetBranchTaken();
            var notTaken = block.GetBranchNotTaken();
            
            foreach (var pass in stage.Passes)
            {
                if (pass.Run(block))
                    continue;

                Logger.Error("BlockPassManager", $"[{stage.Name}] Pass '{pass.GetName()}' failed @ 0x{block.GetAddress():X}.");
                return false;
            }
                
            if (taken != null)
            {
                RunStage(taken, stage);
            }

            if (notTaken != null)
            {
                RunStage(notTaken, stage);
            }
            
            return true;
        }
        
    internal bool Run(BasicBlock block)
    {
            foreach (var stage in _stages)
            {
                Logger.Debug("BlockPassManager", $"Running {stage.Name}");
                
                if (!RunStage(block, stage))
                    return false;
                
                Logger.Debug("BlockPassManager", $"Block count: {stage.BlockList.Count}");
            }

            foreach (var stage in _stages)
                stage.BlockList.Clear();

            return true;
        }
        
    private readonly List<Stage> _stages = [];
}