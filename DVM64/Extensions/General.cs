using System.Collections;

namespace DVM64.Extensions;

public static class General
{
    public static bool InBounds(this ICollection container, int index, int count)
    {
        return index >= 0 &&
               index < container.Count &&
               index + (count - 1) < container.Count;
    }
}