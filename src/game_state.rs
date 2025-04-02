use ndarray::{Array2, ArrayViewMut1, ArrayViewMut2, Ix2, s};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Index;

const BOARD_SIZE: usize = 9;
const BOARD_SHAPE: (usize, usize) = (BOARD_SIZE, BOARD_SIZE);
const BLOCK_SIZE: usize = 3;
const BLOCK_SHAPE: (usize, usize) = (BLOCK_SIZE, BLOCK_SIZE);
const BOARD_BLOCK_COUNT_IN_AXIS: usize = BOARD_SIZE / BLOCK_SIZE;
const BOARD_BLOCK_SHAPE: (usize, usize) = (BOARD_BLOCK_COUNT_IN_AXIS, BOARD_BLOCK_COUNT_IN_AXIS);

#[derive(Clone)]
pub struct GameState {
    board: Array2<BoardCell>,
    history: Vec<Array2<BoardCell>>,
    guesses: Vec<(Ix2, usize)>,
    unfilled_count: usize,
}

impl GameState {
    pub fn try_solve(&mut self) -> Result<GameState, ()> {
        if self.unfilled_count == 0 {
            return Ok(self.clone());
        }

        self.try_solve_without_guesses()?;

        if self.unfilled_count == 0 {
            return Ok(self.clone());
        }

        let mut guess_state = self.clone();
        let (guess_idx, guess) = guess_state.try_guess()?;

        if let Ok(solution) = guess_state.try_solve() {
            return Ok(solution);
        }

        let mut cell = self.board[guess_idx].clone();
        self.board[guess_idx] = match cell {
            BoardCell::Possibilities(mut p) => {
                p.set_impossible(guess);
                BoardCell::Possibilities(p)
            },
            BoardCell::Number(_) => panic!("This should not happen because guesses should be from possibilities"),
        };

        self.try_solve()
    }

    // Tries to solve as much as possible without guessing and returns a false if there is no solution
    fn try_solve_without_guesses(&mut self) -> Result<(), ()> {
        // TODO let planned_moves: VecDeque<Box<dyn Fn()>> = VecDeque::new();
        todo!()
    }

    fn try_guess(&mut self) -> Result<(Ix2, usize), ()> {
        let guess_cell = self
            .board
            .indexed_iter()
            .filter_map(|(idx, cell)| match cell {
                BoardCell::Number(_) => None,
                BoardCell::Possibilities(possibilities) => {
                    Some((idx, possibilities.count(), possibilities))
                }
            })
            .min_by(|(_, first, _), (_, second, _)| first.cmp(second));

        if let Some(((i, j), _, possibilities)) = guess_cell {
            let picked_possibility = possibilities.pick_one().unwrap();
            self.set_cell(Ix2(i, j), BoardCell::Number(picked_possibility));
            let guess = (Ix2(i, j), picked_possibility);
            self.guesses.push(guess);
            Ok(guess)
        } else {
            Err(())
        }
    }

    fn set_cell(&mut self, set_idx: Ix2, cell: BoardCell) {
        self.board[set_idx] = cell.clone();

        // Update possibilities is it's a number cell
        if let BoardCell::Number(number) = cell {
            self.board
                .indexed_iter_mut()
                .filter(|((row, col), _)| {
                    let idx = Ix2(*row, *col);
                    idx.shares_row(set_idx)
                        || idx.shares_column(set_idx)
                        || idx.shares_block(set_idx)
                })
                .for_each(|(_, cell)| {
                    if let BoardCell::Possibilities(p) = cell {
                        p.set_impossible(number)
                    }
                });

            self.unfilled_count -= 1;
        }
    }

    fn get_row_mut(&mut self, idx: Ix2) -> ArrayViewMut1<BoardCell> {
        self.board.row_mut(*idx.index(0))
    }

    fn get_column_mut(&mut self, idx: Ix2) -> ArrayViewMut1<BoardCell> {
        self.board.column_mut(*idx.index(1))
    }

    fn get_block(&mut self, idx: Ix2) -> ArrayViewMut2<BoardCell> {
        let (block_x, block_y) = idx.disassemble();
        let start_x = block_x / BOARD_BLOCK_COUNT_IN_AXIS;
        let start_y = block_y / BOARD_BLOCK_COUNT_IN_AXIS;
        self.board.slice_mut(s![start_x..start_x + BLOCK_SIZE, start_y..start_y + BLOCK_SIZE])
    }

    fn is_complete(&self) -> bool {
        self.board
            .iter()
            .all(|cell| matches!(cell, BoardCell::Number(_)))
    }
}

impl From<&str> for GameState {
    fn from(value: &str) -> Self {
        let initial_board = Array2::from_shape_vec(
            BOARD_SHAPE,
            value
                .chars()
                .filter(|&c| c != '\n')
                .map(|c| BoardCell::try_from(c).expect("Invalid puzzle item"))
                .collect(),
        )
        .expect("Invalid puzzle shape");

        Self {
            unfilled_count: initial_board
                .iter()
                .filter(|cell| matches!(cell, BoardCell::Possibilities(_)))
                .count(),
            board: initial_board.clone(),
            history: vec![initial_board],
            guesses: vec![],
        }
    }
}

impl Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.board)
    }
}

#[derive(Clone, Debug)]
pub enum BoardCell {
    Possibilities(NumberPossibilities),
    Number(usize),
}

impl BoardCell {}

impl TryFrom<char> for BoardCell {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        if value == '_' {
            Ok(BoardCell::Possibilities(NumberPossibilities::default()))
        } else {
            value
                .to_digit(10)
                .map_or(Err(()), |d| Ok(BoardCell::Number(d as usize)))
        }
    }
}

impl Default for BoardCell {
    fn default() -> Self {
        BoardCell::Possibilities(NumberPossibilities::new())
    }
}

impl Display for BoardCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BoardCell::Possibilities(_) => write!(f, "_"),
            BoardCell::Number(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Clone)]
pub struct NumberPossibilities {
    possibilities: [bool; 9],
}

impl NumberPossibilities {
    fn new() -> NumberPossibilities {
        Self {
            possibilities: [true; 9],
        }
    }

    fn is_possible(&self, number: usize) -> bool {
        self.possibilities[number - 1]
    }

    fn set_impossible(&mut self, number: usize) {
        self.possibilities[number - 1] = false;
    }

    fn combine(&mut self, other: NumberPossibilities) {
        for i in 0..self.possibilities.len() {
            self.possibilities[i] = self.possibilities[i] && other.possibilities[i];
        }
    }

    fn evaluate(self) -> Option<BoardCell> {
        match self.count() {
            0 => None,
            1 => Some(BoardCell::Number(
                self.possibilities.iter().position(|&n| n).unwrap(),
            )),
            _ => Some(BoardCell::Possibilities(self)),
        }
    }

    fn count(&self) -> usize {
        self.possibilities.iter().filter(|&&n| n).count()
    }

    // Picks one of the possible options (right now the first one)
    fn pick_one(&self) -> Option<usize> {
        self.possibilities
            .iter()
            .enumerate()
            .find(|(_, possible)| **possible)
            .map(|(idx, _)| idx)
    }
}

impl Default for NumberPossibilities {
    fn default() -> Self {
        NumberPossibilities::new()
    }
}

impl Debug for NumberPossibilities {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut possibilities = vec![];

        for i in 0..self.possibilities.len() {
            if self.possibilities[i] {
                possibilities.push(i + 1);
            }
        }

        possibilities.fmt(f)
    }
}

trait SudokuBoardIndex {
    fn shares_row(&self, other: Ix2) -> bool;
    fn shares_column(&self, other: Ix2) -> bool;
    fn shares_block(&self, other: Ix2) -> bool;
    fn get_block_index(&self) -> Ix2;
    fn disassemble(self) -> (usize, usize);
}

impl SudokuBoardIndex for Ix2 {
    fn shares_row(&self, other: Ix2) -> bool {
        self.index(0) == other.index(0)
    }

    fn shares_column(&self, other: Ix2) -> bool {
        self.index(1) == other.index(1)
    }

    fn shares_block(&self, other: Ix2) -> bool {
        self.get_block_index() == other.get_block_index()
    }

    fn get_block_index(&self) -> Ix2 {
        Ix2(self.index(0) / 3, self.index(1) / 3)
    }

    fn disassemble(self) -> (usize, usize) {
        (*self.index(0), *self.index(1))
    }
}