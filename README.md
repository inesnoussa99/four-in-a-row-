# 🎮 Connect Four in Prolog

A complete Connect Four (Puissance 4) game implemented in Prolog with multiple AI levels, benchmarking tools, and simulation capabilities.

## 📋 Table of Contents

- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [Architecture](#architecture)
- [Artificial Intelligence](#artificial-intelligence)
- [Testing and Benchmarking](#testing-and-benchmarking)
- [Examples](#examples)

## ✨ Features

- **Interactive gameplay**: Human vs Human, Human vs AI, or AI vs AI modes
- **4 AI difficulty levels**:
  - AI v1: Random move selection
  - AI v2: Winning move detection
  - AI Minimax: Depth-limited search with heuristic evaluation
  - AI Alpha-Beta: Optimized minimax with pruning
- **Analysis tools**:
  - Performance benchmarking (time, inferences)
  - Mass game simulation
  - Detailed profiling (nodes explored, pruning count)
- **Comprehensive unit tests**

## 🔧 Prerequisites

- **SWI-Prolog** version 8.0 or higher
- Unix/Linux/macOS system (for shell scripts) or Windows

Installing SWI-Prolog:
```bash
# Ubuntu/Debian
sudo apt-get install swi-prolog

# macOS (Homebrew)
brew install swi-prolog

# Windows
# Download from https://www.swi-prolog.org/download/stable
```

## 📦 Installation

1. Clone or download the project:
```bash
git clone https://github.com/inesnoussa99/four-in-a-row-.git
cd four-in-a-row-
```

2. Verify all files are present:
```
game.pl              # Game logic
ia_v1.pl             # Random AI
ia_v2.pl             # AI with win detection
ia_minimax.pl        # Minimax AI
ia_alphabeta.pl      # Alpha-Beta AI
main.pl              # Entry point
benchmark.pl         # Benchmarking tools
simulator.pl         # Game simulator
profiling.pl         # Performance analysis
test_*.pl            # Unit tests
run_tests.sh         # Test runner script
```

## 🎯 Usage

### Launch an Interactive Game

```bash
swipl -s main.pl
```

Then in the Prolog interpreter:
```prolog
?- choose_player('x', TypeX), choose_player('o', TypeO).
```

Follow the prompts to choose the type for each player (human or AI).

### Simulate Games

```prolog
?- [simulator].
?- simulate_games(ia_v2, ia_minimax, 100, true, Summary).
?- print_summary(Summary).
```

This simulates 100 games between AI v2 (X) and AI Minimax (O) with alternating first player.

### Performance Benchmarking

```prolog
?- [benchmark].
?- benchmark_games(ia_alphabeta, ia_minimax, 50, true, 42, Report).
?- print_report(Report).
```

Measures execution time, number of inferences, and statistics over 50 games.

### Detailed Profiling

```prolog
?- [profiling].
?- profile_minimax(_, 'x', 6, profile(BestCol, Nodes)).
% Shows the number of nodes explored by Minimax

?- profile_alphabeta(_, 'x', 6, 1000000, profile(BestCol, Nodes, Prunes)).
% Shows nodes explored and pruning operations performed
```

## 🏗️ Architecture

### Project Structure

```
├── game.pl              # Game engine (board, moves, victory)
├── main.pl              # User interface
├── ia_*.pl              # Different AI implementations
├── benchmark.pl         # Performance measurement
├── simulator.pl         # Game simulation
├── profiling.pl         # Detailed analysis
└── test_*.pl            # Unit tests
```

### Main Modules

#### `game.pl`
Manages game logic:
- 6x7 board
- Move validation
- Win detection (horizontal, vertical, diagonal)
- Draw detection (full board)

#### `ia_*.pl`
Different AI implementations:
- **v1**: Random selection from valid columns
- **v2**: Searches for winning move, otherwise plays randomly
- **minimax**: Minimax algorithm with heuristic evaluation (depth 4)
- **alphabeta**: Minimax with alpha-beta pruning (depth 6)

#### `benchmark.pl` and `profiling.pl`
Performance analysis tools to compare AI algorithms.

## 🤖 Artificial Intelligence

### Heuristic Evaluation

The Minimax and Alpha-Beta AIs use an evaluation function based on:
- Windows of 4 consecutive cells
- Count of aligned pieces
- Different weights based on piece count:
  - 3 pieces + 1 empty: ±50 points
  - 2 pieces + 2 empty: ±10 points
  - 4 aligned pieces: ±1000 points (victory)

### Optimizations

- **Move ordering**: Center columns are explored first
- **Alpha-beta pruning**: Significantly reduces the search tree
- **Adaptive depth**: Depth 6 for Alpha-Beta vs 4 for Minimax

### Comparative Performance

Based on typical benchmarks:
- **AI v1**: ~0.001s per move
- **AI v2**: ~0.002s per move
- **AI Minimax**: ~0.5-2s per move (depending on position)
- **AI Alpha-Beta**: ~0.1-0.5s per move (3-5x faster than Minimax)

## 🧪 Testing and Benchmarking

### Run All Tests

```bash
chmod +x run_tests.sh
./run_tests.sh
```

Or manually:
```bash
swipl -q -s test_game.pl -g "run_tests, halt"
swipl -q -s test_engine.pl -g "run_tests, halt"
swipl -q -s test_minimax.pl -g "run_tests, halt"
swipl -q -s test_performance.pl -g "run_tests, halt"
swipl -q -s test_alphabeta.pl -g "run_tests, halt"
swipl -q -s test_syntax.pl -g "run_tests, halt"
```

### Available Tests

- `test_game.pl`: Game logic tests
  - Board initialization
  - Move validation
  - Move execution
  - Player switching
  
- `test_engine.pl`: Core mechanics tests
  - AI v2 execution
  - Minimax execution
  
- `test_minimax.pl`: Minimax algorithm tests
  - Center preference on empty board
  - Vertical blocking
  - Immediate win detection
  
- `test_alphabeta.pl`: Alpha-Beta algorithm tests
  - Center strategy
  - Horizontal blocking
  
- `test_performance.pl`: Performance tests
  - Minimax speed validation (< 0.8s for depth 3)
  - Valid move generation
  
- `test_profiling.pl`: Profiling tests
  - Execution time validation
  
- `test_syntax.pl`: Syntax validation
  - All files load correctly
  
- `test_benchmark.pl`: Benchmark tests
  - AI comparison
  
- `test_quit.pl`: Clean exit tests
  - Exception handling

## 📊 Examples

### Example 1: AI vs AI Game

```prolog
?- [simulator].
?- simulate_game(ia_alphabeta, ia_minimax, 'x', Result).
Result = win('x').
```

### Example 2: Comparative Benchmark

```prolog
?- [benchmark].
?- benchmark_games(ia_v2, ia_alphabeta, 20, false, 42, R1).
?- print_report(R1).
=== Benchmark ===
Games        : 20
Total moves  : 142
X wins       : 2
O wins       : 17
Draws        : 1
Aborted      : 0
Time (s)     : 8.432000
Inferences   : 15234567
Avg sec/move: 0.059380
Avg inf/move: 107283.57
```

### Example 3: Profiling Analysis

```prolog
?- [profiling].
?- profile_alphabeta(_, 'x', 6, 1000000, P).
P = profile(4, 1523, 847).
% Best move: column 4
% 1523 nodes explored
% 847 pruning operations performed
```

### Example 4: Mass Simulation

```prolog
?- [simulator].
?- simulate_games(ia_v1, ia_v2, 1000, true, Summary).
?- print_summary(Summary).
=== Simulation summary ===
Games : 1000
X wins: 127
O wins: 843
Draws : 30
```

### Example 5: Interactive Game Setup

```prolog
?- [main].
?- choose_player('x', TypeX), choose_player('o', TypeO).
Configure x :
  1. Human
  2. IA v1 (random)
  3. IA v2 (winning move if possible)
  4. IA minimax
  5. IA alpha-beta
Your choice: 1.
Configure o :
  1. Human
  2. IA v1 (random)
  3. IA v2 (winning move if possible)
  4. IA minimax
  5. IA alpha-beta
Your choice: 5.
TypeX = human,
TypeO = ia_alphabeta.
```

## 🔍 Advanced Features

### Custom Board Analysis

You can analyze specific board positions:

```prolog
?- [profiling].
?- Board = [
    ['x','o','x','.','.','.','.'],
    ['o','x','o','.','.','.','.'],
    ['x','o','x','.','.','.','.'],
    ['.','.','.','.','.','.','.'
    ['.','.','.','.','.','.','.'
    ['.','.','.','.','.','.','.']
],
profile_alphabeta(Board, 'x', 6, 1000000, P).
```

### Benchmark with Move Limit

Prevent infinite games with a move limit:

```prolog
?- benchmark_games(ia_v1, ia_v1, 100, false, 30, Report).
% Stops each game after 30 moves max
```

### Alternating First Player

Simulate fairer matches by alternating who plays first:

```prolog
?- simulate_games(ia_minimax, ia_alphabeta, 100, true, Summary).
% true = alternate starting player
```

## 📈 Performance Tips

1. **Depth adjustment**: Modify search depth in AI files for faster/stronger play
   - `ia_minimax.pl`: Change `depth(4)` predicate
   - `ia_alphabeta.pl`: Change `search_depth(6)` predicate

2. **Disable output**: Comment out `write` statements in AI files for faster benchmarking

3. **Profiling**: Use `profiling.pl` to identify bottlenecks

## 🤝 Contributing

Contributions are welcome! To contribute:

1. Fork the project
2. Create a feature branch (`git checkout -b feature/improvement`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin feature/improvement`)
5. Open a Pull Request

## 👥 Authors

- Ines Chebbi, Hamza El Karchouni, Wassim Bouras, Malek Aouadi, Ilyes Bellargui, Adam Naji, Mario Senah.

## 🙏 Acknowledgments

- Inspiration: The classic Connect Four game
- Algorithms: Minimax and Alpha-Beta from game theory
- SWI-Prolog: For the excellent development environment

## 🐛 Known Issues

- AI Minimax is optimized for player 'o' - when playing as 'x', board is swapped
- Very deep searches (depth > 8) may be slow on older hardware
- Terminal display may vary depending on OS

