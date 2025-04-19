use crate::game_state::GameState;
use std::fs;

mod game_state;

fn main() {
    let puzzle_file_text =
        fs::read_to_string("./puzzles/1.txt").expect("Should have been able to read the file");

    let mut game = GameState::from(puzzle_file_text.as_str());
    println!("{}", game);

    match game.try_solve() {
        Ok(solution) => {
            println!("Solved the puzzle");
            println!("{}", solution);
        }
        Err(_) => {
            println!("Failed to solve the puzzle");
            println!("{}", game);
        }
    };
}
