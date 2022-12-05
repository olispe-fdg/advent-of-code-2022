import os
import readline { read_line }

enum Play as u8 {
	rock
	paper
	scissors
}

const rules = {
	Play.rock: Play.scissors
	Play.paper: Play.rock
	Play.scissors: Play.paper
}

fn a_beats_b(a Play, b Play) bool {
	return rules[a] == b
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
	return match true {
		a_beats_b(opponent, you) { 0 }
		opponent == you { 3 }
		else { 6 }
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
