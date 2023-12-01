internal class Program
{
    private static IEnumerable<string> ReadLines() {
        var x = Console.ReadLine();
        while (x != null) {
            yield return x;
            x = Console.ReadLine();
        }
    }

    private static string part1(IEnumerable<string> lines) {
        var output = 0;

        foreach (var line in lines)
        {
            var first = line.First(x => char.IsAsciiDigit(x));
            var last = line.Last(x => char.IsAsciiDigit(x));

            output += int.Parse($"{first}{last}");
        }

        return output.ToString();
    }

    private static Dictionary<String, String> intMap = new Dictionary<String, String>() {
        {"one", "1"},
        {"two", "2"},
        {"three", "3"},
        {"four", "4"},
        {"five", "5"},
        {"six", "6"},
        {"seven", "7"},
        {"eight", "8"},
        {"nine", "9"},
    };

    private static string part2(IEnumerable<string> lines) {
        var output = 0;

        foreach (var line in lines)
        {
            string? first = null;
            string? last = null;

            for(var i = 0; i < line.Length; i++) {
                var j = line.Length - i;
                first ??= char.IsAsciiDigit(line[i]) ? $"{line[i]}" : getWrittenInt(line[i..], false);
                last ??= char.IsAsciiDigit(line[j-1]) ? $"{line[j-1]}" : getWrittenInt(line[0..j], true);
            }

            output += int.Parse($"{first}{last}");
        }

        return output.ToString();
    }

    private static string? getWrittenInt(string line, bool reverse)
    {
        foreach(var (key, value) in intMap) {
            var matchEnd = reverse && line.EndsWith(key);
            var matchStart = (!reverse) && line.StartsWith(key);
            if (matchEnd || matchStart) {
                return value;
            }
        }
        return null;
    }

    private static void Main(string[] args)
    {
        if(args.Length < 1) { throw new ArgumentException("Need part indicator"); }
        var lines = ReadLines();

        String solution;

        switch (args[0]) {
            case "1": solution = part1(lines); break;
            case "2": solution = part2(lines); break;
            default: throw new ArgumentException("Not part 1 or 2");
        }

        Console.WriteLine(solution);
    }
}