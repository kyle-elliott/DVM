using System.Diagnostics;

namespace DVM64;

internal static class Program
{
    internal static void Main()
    {
        Logger.Initialize();

        try
        {
            Stopwatch stopwatch = new();

            stopwatch.Start();
            Dvm64.Run(Settings.PathToBinary, Settings.RelativeAddress);
            stopwatch.Stop();

            Logger.Info("Program", $"Execution time: {stopwatch.ElapsedMilliseconds}ms");
            Logger.Info("Program", "Press any key to exit.");

            Console.ReadKey();
        }
        catch (Exception exception)
        {
            Logger.Error("Program", $"{exception.Message} {exception.StackTrace}");
        }
    }
}