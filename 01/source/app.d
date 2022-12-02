import std.stdio;
import core.stdc.errno;
import std.exception;

void main(string[] args)
{
	if (args.length < 2) {
		writeln("No file specified");
		return;
	}

	try {
		auto file = File(args[1], "r");
		
		foreach (line; file.byLine) {
			writeln(line);
		}
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
	}
}
