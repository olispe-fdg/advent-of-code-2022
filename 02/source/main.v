import os
import readline { read_line }

enum Play as u8 {
	rock
	paper
	scissors
}

enum Result as u8 {
	lose
	draw
	win
}

const wins = {
	Play.rock: Play.scissors
	Play.paper: Play.rock
	Play.scissors: Play.paper
}

const loses = {
	Play.scissors: Play.rock
	Play.rock: Play.paper
	Play.paper: Play.scissors
}

fn a_beats_b(a Play, b Play) bool {
	return wins[a] == b
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

fn number_to_result(num u8) Result {
	return match num {
		0 { Result.lose }
		1 { Result.draw }
		2 { Result.win }
		else {
			assert false
			Result.lose
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

fn part1(column1 u8, column2 u8) u8 {
	opponent, you := number_to_play(column1), number_to_play(column2)
	return play_score(you) + round_score(opponent, you)
}

fn part2(column1 u8, column2 u8) u8 {
	opponent, desired_result := number_to_play(column1), number_to_result(column2)
	you := match desired_result {
		.lose { wins[opponent] }
		.draw { opponent }
		.win { loses[opponent] }
	}

	return play_score(you) + round_score(opponent, you)
}

fn main() {
	input_filename := os.args[1]

	mut total_score_part1, mut total_score_part2 := 0, 0
	lines := os.read_lines(input_filename)!
	for line in lines {
		column1, column2 := line[0] - u8(`A`), line[2] - u8(`X`)

		total_score_part1 += part1(column1, column2)
		total_score_part2 += part2(column1, column2)
	}

	println(total_score_part1)
	println(total_score_part2)
	read_line("Press enter to exit...\n")!
}
