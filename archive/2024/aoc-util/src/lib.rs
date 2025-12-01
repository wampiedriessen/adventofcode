pub trait PuzzleDay {
    fn new(input: &str) -> Self;

    fn solve(&mut self) -> Result<(String, String), String>;
}
