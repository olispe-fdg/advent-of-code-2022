import std.stdio;
import core.stdc.errno;
import std.exception;
import std.conv;
import std.algorithm;

int main(string[] args)
{
	if (args.length < 2) {
		writeln("No file specified");
		return 1;
	}

	try {
		auto file = File(args[1], "r");

		uint[] elves;
		uint caloriesAccumulator = 0;

		foreach (line; file.byLine) {
			if (line.length == 0) {
				elves ~= caloriesAccumulator;
				caloriesAccumulator = 0;
				continue;
			}

			try {
				const uint calories = to!uint(line);
				caloriesAccumulator += calories;
			} catch (ConvException err) {
				writeln("Failed to parse input file");
				return 1;
			}
		}
		file.close(); // D does cleanup but we're hanging the terminal later

		if (elves.length < 3) {
			writeln("Not enough elves");
			return 1;
		}
		elves.sort!("a > b");

		writefln("Top 3 elves: %u, %u, %u", elves[0], elves[1], elves[2]);
		writefln("Total: %u", elves[0] + elves[1] + elves[2]);

		writeln("Press enter to exit...");
		getchar();
	} catch (ErrnoException err) {
		switch (err.errno) {
			case EPERM:
			case EACCES:
				writeln("Could not open input file: Permission denied");
				break;
			case ENOENT:
				writeln("Could not open input file: File does not exist");
				break;
			default:
				writeln("Could not open input file: Unknown error occured");
				break;
		}

		return 1;
	}

	return 0;
}
