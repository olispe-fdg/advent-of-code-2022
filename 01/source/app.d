import std.stdio;
import core.stdc.errno;
import std.exception;
import std.conv;

int main(string[] args)
{
	if (args.length < 2) {
		writeln("No file specified");
		return 1;
	}

	try {
		auto file = File(args[1], "r");

		uint maxCalories = 0;
		uint caloriesAccumulator = 0;

		foreach (line; file.byLine) {
			if (line.length == 0) {
				maxCalories = caloriesAccumulator > maxCalories ? caloriesAccumulator : maxCalories;
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

		writefln("Maximum total calories: %u", maxCalories);
		writeln("Press any key to exit...");

		file.close(); // D does cleanup but we're hanging the terminal
		getchar(); // Pause the terminal
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
