# Connect Four in a Row — Versions 2 & 3

This project contains two implementations of the classic **Connect Four** game written in **Prolog**. It demonstrates the evolution of a game engine from a **random opponent (v2)** to a **heuristic "smart" agent (v3)**, along with improvements in **user input handling**.

---

## 📂 Project Overview

| Version | AI Strategy                          | Input Style            | Best For...                                     |
| ------- | ------------------------------------ | ---------------------- | ----------------------------------------------- |
| **v2**  | Random: plays in any valid column    | Standard Prolog (`4.`) | Testing game mechanics and basic logic          |
| **v3**  | Heuristic: attacks, blocks, strategy | Natural (`4 + Enter`)  | Playing a competitive game against the computer |

---

## 🚀 How to Run

### Prerequisites

You need a Prolog interpreter installed.
**Recommended:** [SWI-Prolog](https://www.swi-prolog.org/)

### Installation

Save the two versions into separate files:

```
puissance4_v2.pl
puissance4_v3.pl
```

### Launching the Game

```bash
swipl puissance4_v3.pl
```

Then start the game:

```prolog
?- init.
```

---

## 🧠 Version Details

### Version 2 — The Baseline

**File:** `puissance4_v2.pl`

This version establishes the **core mechanics** of the game.

#### 🟡 AI Logic

The AI uses:

```prolog
random_between(1,7,Col)
```

It simply checks if a column is valid — it has **no concept of winning**, blocking, or strategy.

#### ⌨️ Input

Uses standard `read/1`.
To play column **3**, type:

```
3.
```

Then press **Enter**.

---

### Version 3 — The "Smart" Upgrade

**File:** `puissance4_v3.pl`

This version focuses on **game experience and competitive AI**.

#### 🎯 1. Heuristic AI

The predicate `ia/2` follows a strict **priority list** using `!` cuts to pick the optimal move:

1. **Instant Win**

   * If AI can win this turn → it plays the winning move.
2. **Block Opponent**

   * If human can win next turn → AI blocks.
3. **Control Center**

   * Otherwise chooses moves from:

     ```
     [4,3,5,2,6,1,7]
     ```
   * Center columns are statistically stronger in Connect Four.

---

#### 🧾 2. Better Input Handling

Replaces `read/1` with `read_line_to_codes/2`.

**Benefits**

* No need for periods: just type `4` and hit Enter
* Handles invalid/non-numeric input safely
* Prevents accidental crashes

---

## 🛠 Shared Architecture (Both Versions)

Both versions use the same **backend logic**:

### 1. Board Representation

A 6×7 board is stored as a dynamic predicate:

```prolog
:- dynamic board/1.
```

Board updates use `retract/1` and `assert/1` through `applyIt/2`.

---

### 2. Gravity Simulation

Prolog lists don’t support **gravity**, so `playMove/4` simulates it:

* `valid_col/2`: checks if a column is playable
* `find_free_row/3`: scans from bottom up
* `replace/4`: inserts token into row list

---

### 3. Win Detection

The `gameover/2` predicate checks four directions:

* **Horizontal**: `win_h`
* **Vertical**: `win_v` (using list transpose)
* **Diagonal /**: `win_d1`
* **Diagonal /**: `win_d2`

All win conditions use a shared predicate: `adjacent4/1`.

