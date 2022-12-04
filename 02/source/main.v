import os
import readline { read_line }

enum Play as u8 {
	rock
	paper
	scissors
}

fn number_to_play(num u8) Play {
	return match num {
		0 { Play.rock }
		1 { Play.paper }
		2 { Play.scissors }
		else {
			assert false
			Play.rock
		}
	}
}

fn play_score(play Play) u8 {
	return match play {
		.rock { 1 }
		.paper { 2 }
		.scissors { 3 }
	}
}

fn round_score(opponent Play, you Play) u8 {
	return match opponent {
		.rock {
			match you {
				.rock { 3 }
				.paper { 6 }
				.scissors { 0 }
			}
		}
		.paper {
			match you {
				.rock { 0 }
				.paper { 3 }
				.scissors { 6 }
			}
		}
		.scissors {
			match you {
				.rock { 6 }
				.paper { 0 }
				.scissors { 3 }
			}
		}
	}
}

fn main() {
	input_filename := os.args[1]

	mut total_score := 0
	lines := os.read_lines(input_filename)!
	for line in lines {
		opponent, you := number_to_play(line[0] - u8(`A`)), number_to_play(line[2] - u8(`X`))
		total_score += play_score(you) + round_score(opponent, you)
	}

	println(total_score)
	read_line("Press enter to exit...\n")!
}
