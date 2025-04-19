use ndarray::{Array2, ArrayViewMut1, ArrayViewMut2, Ix, Ix2, s};
use std::cmp::PartialEq;
use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::ops::Index;

const BOARD_SIZE: usize = 9;
const BOARD_SHAPE: (usize, usize) = (BOARD_SIZE, BOARD_SIZE);
const BLOCK_SIZE: usize = 3;

#[derive(Clone, Debug)]
pub struct GameState {
    board: Array2<BoardCell>,
    history: Vec<Array2<BoardCell>>,
    guesses: Vec<(Ix2, usize)>,
    unfilled_count: usize,
    areas_to_check: VecDeque<BoardArea>,
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

        println!("{}", self.board);

        let mut guess_state = self.clone();
        let (guess_idx, guess) = guess_state.try_guess()?;

        if let Ok(solution) = guess_state.try_solve() {
            return Ok(solution);
        }

        self.set_failed_guess(guess_idx, guess);

        self.try_solve()
    }

    // Tries to solve as much as possible without guessing and returns a false if there is no solution
    fn try_solve_without_guesses(&mut self) -> Result<(), ()> {
        loop {
            let area = match self.areas_to_check.pop_front() {
                None => {
                    break;
                }
                Some(area) => area,
            };

            let cells: Vec<_> = match area {
                BoardArea::Row(idx) => self.get_row_mut(idx).to_vec(),
                BoardArea::Column(idx) => self.get_column_mut(idx).to_vec(),
                BoardArea::Block(idx) => self.get_block_mut(idx).flatten().to_vec(),
            };

            let area_possibilities =
                cells
                    .iter()
                    .fold(NumberPossibilities::default(), |mut acc, cell| {
                        if let BoardCell::Number(num) = cell {
                            acc.set_impossible(*num);
                        }

                        acc
                    });

            let area_items: Vec<(Ix2, BoardCell)> = match area {
                BoardArea::Row(idx) => self
                    .get_row_mut(idx)
                    .indexed_iter_mut()
                    .map(|(col, x)| (area.global_index_of(&col), x.clone()))
                    .collect(),
                BoardArea::Column(idx) => self
                    .get_column_mut(idx)
                    .indexed_iter_mut()
                    .map(|(row, x)| (area.global_index_of(&row), x.clone()))
                    .collect(),
                BoardArea::Block(idx) => self
                    .get_block_mut(idx)
                    .flatten()
                    .indexed_iter_mut()
                    .map(|(idx2, x)| (area.global_index_of(&idx2), x.clone()))
                    .collect(),
            };

            if area_possibilities.count() == 0 {
                continue;
            }

            area_items.iter().try_for_each(|(idx, cell)| {
                if let BoardCell::Possibilities(possibilities) = cell {
                    let mut possibilities = possibilities.clone();
                    possibilities.combine(&area_possibilities);

                    if let BoardCell::Number(num) = possibilities.evaluate()? {
                        self.set_cell(*idx, BoardCell::Number(num));
                    }
                }

                Ok::<(), ()>(())
            })?
        }

        // TODO: Maybe add more checks
        Ok(())
    }

    fn try_guess(&mut self) -> Result<(Ix2, usize), ()> {
        let guess_cell = self
            .board
            .indexed_iter()
            .filter_map(|((x, y), cell)| match cell {
                BoardCell::Number(_) => None,
                BoardCell::Possibilities(possibilities) => {
                    Some((Ix2(x, y), possibilities.count(), possibilities))
                }
            })
            .min_by(|(_, first, _), (_, second, _)| first.cmp(second));

        if let Some((idx, _, possibilities)) = guess_cell {
            let picked_possibility = possibilities.pick_one().unwrap();
            self.set_cell(idx, BoardCell::Number(picked_possibility));
            let guess = (idx, picked_possibility);
            self.guesses.push(guess);
            Ok(guess)
        } else {
            Err(())
        }
    }

    fn set_cell(&mut self, set_idx: Ix2, cell: BoardCell) {
        if self.board[set_idx] == cell {
            return;
        }
        if matches!(self.board[set_idx], BoardCell::Number(_)) {
            println!("{}", self.board);
            panic!();
        }
        self.board[set_idx] = cell.clone();

        // Update possibilities if it is a number cell
        if let BoardCell::Number(number) = cell {
            self.unfilled_count -= 1;

            fn set_impossible(state: &mut GameState, index: Ix2, number: usize) {
                let mut p = if let BoardCell::Possibilities(p) = &mut state.board[index] {
                    p.clone()
                } else {
                    return;
                };

                p.set_impossible(number);
                state.set_cell(index, p.evaluate().expect("Cell had 0 possibilities"));
            }

            for idx in 0..BOARD_SIZE {
                set_impossible(self, BoardArea::Row(set_idx).global_index_of(&idx), number);
                set_impossible(
                    self,
                    BoardArea::Column(set_idx).global_index_of(&idx),
                    number,
                );
                set_impossible(
                    self,
                    BoardArea::Block(set_idx.get_block_index()).global_index_of(&idx),
                    number,
                );
            }

            set_idx.get_areas().iter().for_each(|&area| {
                if !self.areas_to_check.contains(&area) {
                    self.areas_to_check.push_back(area)
                }
            });
        }
    }

    fn set_failed_guess(&mut self, guess_idx: Ix2, guess: usize) {
        let cell = self.board[guess_idx].clone();
        self.board[guess_idx] = match cell {
            BoardCell::Possibilities(mut p) => {
                p.set_impossible(guess);
                BoardCell::Possibilities(p)
            }
            BoardCell::Number(_) => {
                panic!("This should not happen because guesses should be from possibilities")
            }
        };

        guess_idx.get_areas().iter().for_each(|&area| {
            if !self.areas_to_check.contains(&area) {
                self.areas_to_check.push_back(area)
            }
        });
    }

    fn get_row_mut(&mut self, idx: Ix2) -> ArrayViewMut1<BoardCell> {
        self.board.row_mut(*idx.index(0))
    }

    fn get_column_mut(&mut self, idx: Ix2) -> ArrayViewMut1<BoardCell> {
        self.board.column_mut(*idx.index(1))
    }

    fn get_block_mut(&mut self, idx: BlockIx) -> ArrayViewMut2<BoardCell> {
        let (block_x, block_y) = idx.disassemble();
        let start_x = block_x * BLOCK_SIZE;
        let start_y = block_y * BLOCK_SIZE;
        self.board.slice_mut(s![
            start_x..start_x + BLOCK_SIZE,
            start_y..start_y + BLOCK_SIZE
        ])
    }
}

impl From<&str> for GameState {
    fn from(value: &str) -> Self {
        let initial_board = Array2::from_shape_simple_fn(BOARD_SHAPE, BoardCell::default);
        let mut initial_state = Self {
            board: initial_board,
            history: vec![],
            guesses: vec![],
            unfilled_count: BOARD_SIZE.pow(2),
            areas_to_check: VecDeque::new(),
        };

        // Update the default board according to the loaded values
        let loaded_board = Array2::from_shape_vec(
            BOARD_SHAPE,
            value
                .chars()
                .filter(|&c| c != '\n')
                .map(|c| BoardCell::try_from(c).expect("Invalid puzzle item"))
                .collect(),
        )
        .expect("Invalid puzzle shape");

        loaded_board
            .indexed_iter()
            .filter(|(_, cell)| matches!(cell, BoardCell::Number(_)))
            .for_each(|((x, y), cell)| {
                initial_state.set_cell(Ix2(x, y), cell.clone());
            });

        println!("Initial state:\n{}", initial_state);

        initial_state.history.push(initial_state.board.clone());

        initial_state
    }
}

impl Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Guesses: {:?}\n{}", self.guesses, self.board)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BoardArea {
    Row(Ix2),
    Column(Ix2),
    Block(BlockIx),
}

impl BoardArea {
    fn global_index_of(&self, idx: &Ix) -> Ix2 {
        match self {
            BoardArea::Row(area_idx) => Ix2(area_idx[0], *idx),
            BoardArea::Column(area_idx) => Ix2(*idx, area_idx[1]),
            BoardArea::Block(area_idx) => get_global_idx(area_idx, idx),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BoardCell {
    Possibilities(NumberPossibilities),
    Number(usize),
}

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
        BoardCell::Possibilities(NumberPossibilities::default())
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

#[derive(Clone, Eq, PartialEq)]
pub struct NumberPossibilities {
    possibilities: [bool; 9],
}

impl NumberPossibilities {
    fn set_impossible(&mut self, number: usize) {
        self.possibilities[number - 1] = false;
    }

    fn combine(&mut self, other: &NumberPossibilities) {
        for i in 0..self.possibilities.len() {
            self.possibilities[i] = self.possibilities[i] && other.possibilities[i];
        }
    }

    fn evaluate(self) -> Result<BoardCell, ()> {
        match self.count() {
            0 => Err(()),
            1 => Ok(BoardCell::Number(
                self.possibilities.iter().position(|&n| n).unwrap() + 1,
            )),
            _ => Ok(BoardCell::Possibilities(self)),
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
            .map(|(idx, _)| idx + 1)
    }
}

impl Default for NumberPossibilities {
    fn default() -> Self {
        Self {
            possibilities: [true; 9],
        }
    }
}

impl Debug for NumberPossibilities {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    fn get_block_index(&self) -> BlockIx;
    fn get_areas(&self) -> Vec<BoardArea>;
    fn disassemble(self) -> (usize, usize);
}

impl SudokuBoardIndex for Ix2 {
    fn get_block_index(&self) -> BlockIx {
        BlockIx(Ix2(self.index(0) / BLOCK_SIZE, self.index(1) / BLOCK_SIZE))
    }

    fn get_areas(&self) -> Vec<BoardArea> {
        vec![
            BoardArea::Row(*self),
            BoardArea::Column(*self),
            BoardArea::Block(self.get_block_index()),
        ]
    }

    fn disassemble(self) -> (usize, usize) {
        (*self.index(0), *self.index(1))
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct BlockIx(Ix2);

impl BlockIx {
    fn disassemble(self) -> (Ix, Ix) {
        self.0.disassemble()
    }
}

impl Index<usize> for BlockIx {
    type Output = Ix;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

fn get_global_idx(block_idx: &BlockIx, idx: &Ix) -> Ix2 {
    Ix2(
        block_idx[0] * BLOCK_SIZE + idx % BLOCK_SIZE,
        block_idx[1] * BLOCK_SIZE + idx / BLOCK_SIZE,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_simple_state() -> GameState {
        GameState::from(
            "123456789\
        987654321\
        123456789\
        987654321\
        123456789\
        987654321\
        123456789\
        987654321\
        123456789",
        )
    }

    #[test]
    fn get_block_mut_test() {
        let mut state = create_simple_state();
        let block_items: Vec<usize> = state
            .get_block_mut(Ix2(4, 4).get_block_index())
            .flatten()
            .iter()
            .map(|x| {
                if let BoardCell::Number(n) = x {
                    *n
                } else {
                    panic!()
                }
            })
            .collect();

        let expected = vec![6, 5, 4, 4, 5, 6, 6, 5, 4];

        assert_eq!(block_items, expected);
    }

    #[test]
    fn get_global_index_test() {
        let mut state = create_simple_state();

        let row = BoardArea::Row(Ix2(2, 6));
        let cell_in_row = if let BoardCell::Number(n) = state.board[row.global_index_of(&7)] {
            n
        } else {
            panic!()
        };

        assert_eq!(cell_in_row, 8);

        for idx in 0..BOARD_SIZE {
            assert_eq!(BoardArea::Row(Ix2(2, 6)).global_index_of(&idx), Ix2(2, idx));
            assert_eq!(
                BoardArea::Column(Ix2(2, 6)).global_index_of(&idx),
                Ix2(idx, 6)
            );
            assert_eq!(
                BoardArea::Block(Ix2(2, 6).get_block_index()).global_index_of(&idx),
                Ix2(0 + idx % BLOCK_SIZE, 6 + idx / BLOCK_SIZE)
            );
        }
    }
}

