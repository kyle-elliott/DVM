using System.Reflection.PortableExecutable;
using DVM64.Explorer;

namespace DVM64;

internal static class Dvm64
{
    // "The wicked plot against the righteous and gnash their teeth at them;
    // but the Lord laughs at the wicked, for he knows their day is coming."
    // - Psalm 37:12-13

    internal static void Run(string binaryPath, ulong rva)
    {
        using FileStream file = new(binaryPath, FileMode.Open, FileAccess.Read, FileShare.Read);
        using PEReader pe = new(file);

        // Check if the PE header is present; one should always be there
        if (pe.PEHeaders.PEHeader == null)
        {
            Logger.Error("DVM64", "PE header is missing.");
            return;
        }

        var imageBase = pe.PEHeaders.PEHeader.ImageBase;
        var entry = imageBase + rva;

        Logger.Info("DVM64", $"Image base @ 0x{imageBase:X}");
        Logger.Info("DVM64", $"Interpreter @ 0x{entry:X}");

        // Create a new X86 lifter and pass manager
        X86.Lifter lifter = new(pe, imageBase);
        X86.BlockPassManager passManager = new();

        // Lift the interpreter to a basic block
        var basicBlock = lifter.Lift(entry);

        if (basicBlock == null)
        {
            Logger.Error("DVM64", "Failed to lift entry block!");
            return;
        }

        // Queue up passes and run them on the basic block
        passManager.Register([
            new X86.BlockPassManager.Stage("Stage 1", [
                new X86.Passes.BranchCoalesce()
            ])
        ]);

        if (!passManager.Run(basicBlock))
        {
            Logger.Error("DVM64", "Failed to run pass.");
            return;
        }

        // Build a Tracer and build the VmEnter
        Tracer tracer = new(lifter.Reader, entry);

        if (!tracer.TraceBlock(basicBlock))
        {
            Logger.Error("DVM64", "Failed to analyze the VMEnter.");
            return;
        }

        Logger.Info("DVM64", "Traced entry block");

        var handlerBlock = basicBlock;
        while (true)
        {
            var handlerAddr = tracer.GetHandlerAddress(handlerBlock);
            if (handlerAddr == null)
            {
                Logger.Info("DVM64", "Finished tracing");
                break;
            }

            Logger.Info("DVM64", $"Tracing handler @ 0x{handlerAddr:X}");

            if (handlerAddr == 0)
            {
                Logger.Error("DVM64", "Failed to analyze handlers.");
                return;
            }

            handlerBlock = lifter.Lift(handlerAddr.Value);
            if (handlerBlock == null)
            {
                Logger.Error("DVM64", "Failed to lift handler block!");
                return;
            }

            if (!passManager.Run(handlerBlock))
            {
                Logger.Error("DVM64", "Failed to run pass.");
                return;
            }

            if (!tracer.TraceBlock(handlerBlock))
            {
                Logger.Error("DVM64", "Failed to analyze the VMEnter.");
                return;
            }
        }

        Logger.Info("DVM64", $"Context: {tracer.ExecContext}\n");
    }
}