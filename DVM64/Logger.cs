using Serilog;
using Serilog.Sinks.SystemConsole.Themes;

namespace DVM64;

internal static class Logger
{
    private static Serilog.Core.Logger? _logger;

    internal static void Initialize()
    {
        _logger = new LoggerConfiguration()
            .WriteTo
            .Console(theme: AnsiConsoleTheme.Code)
            .CreateLogger();
    }

    internal static void Debug(string unit, string message)
    {
        _logger?.Debug($"{unit}: {message}");
    }

    internal static void Info(string unit, string message)
    {
        _logger?.Information($"{unit}: {message}");
    }

    internal static void Warning(string unit, string message)
    {
        _logger?.Warning($"{unit}: {message}");
    }

    internal static void Error(string unit, string message)
    {
        _logger?.Error($"{unit}: {message}");
    }
}