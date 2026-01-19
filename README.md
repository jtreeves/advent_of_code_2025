# Advent of Code 2025

This repository contains solutions to the Advent of Code 2025 challenge problems. Each day's problem is solved using 12 different programming languages.

## Structure

- `days/` - Contains folders for each day (day_01 through day_12)
  - Each day folder contains:
    - `README.md` - Problem description and key info from AOC
    - `ANALYSIS.md` - Overall approach and language implementation differences
    - `data/` - Input files (`input.txt` is gitignored, `test_1.txt`, `test_2.txt`, etc. are committed)
    - `solutions/` - Solution files for each language
    - `specs/` - Spec-kit specifications, plans, and tasks (per-day)
- `.cursor/` - Cursor IDE configurations (commands, rules, documentation)
- `.specify/` - Spec-kit configurations (templates, scripts, memory)

## Languages Used

- C
- Clojure
- Elixir
- Go
- Haskell
- Java
- Julia
- Kotlin
- Python
- Ruby
- Rust
- TypeScript

## Setup

### Prerequisites

This project requires the following language runtimes/compilers. Some are already configured in the repo:

#### Already Configured

- **TypeScript**: Configured with `package.json` and `tsconfig.json`
  - Install dependencies: `npm install`
- **Go**: Configured with `go.mod`
  - Go 1.21+ required

#### Additional Requirements

You'll need to install the following based on which days you want to run:

- **C**: `gcc` or `clang` compiler
- **Clojure**: Clojure CLI tools - [Install Clojure](https://clojure.org/guides/install_clojure)
- **Elixir**: Elixir 1.12+ - [Install Elixir](https://elixir-lang.org/install.html)
- **Haskell**: GHC (Glasgow Haskell Compiler) - [Install GHC](https://www.haskell.org/ghc/)
- **Java**: JDK 11+ (javac and java commands)
- **Julia**: Julia 1.8+ - [Install Julia](https://julialang.org/downloads/)
- **Kotlin**: Kotlin compiler - [Install Kotlin](https://kotlinlang.org/docs/command-line.html)
- **Python**: Python 3.8+ with `typing` support
- **Ruby**: Ruby 2.7+ (usually pre-installed on macOS/Linux)
- **Rust**: Rust toolchain (rustc, cargo) - [Install Rust](https://www.rust-lang.org/tools/install)

### Environment Configuration

#### Session Cookie Setup

For AOC integration (fetching problem descriptions, input files, submitting answers):

1. Run the session cookie script:
   ```bash
   .specify/scripts/bash/get_session_cookie.sh
   ```
2. This will prompt you for your AOC session cookie if not found
3. Cookie is stored in `.env` file (gitignored)

#### TypeScript Setup

```bash
npm install
```

This installs TypeScript compiler and Node.js types.

#### Go Setup

Go module is already configured. Just ensure Go 1.21+ is installed and `go` is in your PATH.

### Spec-Kit Setup (Optional)

This project uses [spec-kit](https://github.com/github/spec-kit) for Spec-Driven Development (SDD).

#### Initializing spec-kit

```bash
uvx --from git+https://github.com/github/spec-kit.git specify init . --ai cursor
```

## Usage

### Cursor Commands

The repository includes several Cursor commands for automating workflows:

#### Main Commands

- `/day <NN>` - Master command that orchestrates the full day workflow:
  1. Get problem description and populate README.md
  2. Get input content
  3. Create specs
  4. Generate solutions in all 12 languages
  5. Run solutions and get answers
  6. Submit answers to AOC
  7. Write ANALYSIS.md
  8. Push to GitHub

#### Individual Commands

- `/get_description [day]` - Fetch problem description, populate README.md
- `/get_input [day]` - Download input for the day
- `/create_specs [day]` - Generate spec-kit artifacts for the day
- `/generate_solutions [day]` - Generate code in all 12 languages
- `/run_solutions [day]` - Run solutions, get answers, update README
- `/submit_answer <day> <answer> [part]` - Submit answer to AOC
- `/push_to_gh [message]` - Push code to GitHub
- `/sync_specs [day]` - Adjust spec files to match code
- `/write_analysis [day]` - Generate ANALYSIS.md

#### Quick Command for Repository Users

- `/quick_solve [day]` - For users pulling the repo with existing solutions:
  1. Get their own input
  2. Run solutions
  3. Submit answers
  4. Push to git

### Running Solutions

All solutions are located in `days/day_NN/solutions/solution.{ext}`. Solutions perform file I/O operations inline.

**Important**: Solutions must be run from within the `solutions/` directory to ensure correct relative paths to `../data/input.txt`. As a result, the first step is always `cd days/day_NN/solutions`.

#### C

```bash
gcc solution.c -o solution
./solution
```

#### Clojure

```bash
clojure solution.clj
```

**Requirements**: Clojure CLI tools - [Install Clojure](https://clojure.org/guides/install_clojure).

#### Elixir

```bash
elixir solution.ex
```

**Requirements**: Elixir 1.12+ - [Install Elixir](https://elixir-lang.org/install.html).

#### Go

```bash
go run solution.go
```

**Requirements**: Go 1.21+ (configured via `go.mod`).

#### Haskell

```bash
ghc solution.hs -o solution
./solution
```

**Requirements**: GHC (Glasgow Haskell Compiler) - [Install GHC](https://www.haskell.org/ghc/).

#### Java

```bash
javac solution.java
java Solution
```

**On Windows**:
```bash
java Solution
```

**Requirements**: JDK 11+ with `javac` and `java` in PATH.

#### Julia

```bash
julia solution.jl
```

**Requirements**: Julia 1.8+ - [Install Julia](https://julialang.org/downloads/).

#### Kotlin

```bash
kotlinc solution.kt -include-runtime -d solution.jar
java -jar solution.jar
```

**Requirements**: Kotlin compiler - [Install Kotlin](https://kotlinlang.org/docs/command-line.html).

#### Python

```bash
python solution.py
```

**Requirements**: Python 3.8+ with `typing` support (standard library).

#### Ruby

```bash
ruby solution.rb
```

**Requirements**: Ruby 2.7+ (usually pre-installed on macOS/Linux).

#### Rust

```bash
rustc solution.rs --edition 2021
./solution
```

**Alternative**: Use Cargo (if Cargo.toml is present):
```bash
cargo run
```

**Requirements**: Rust toolchain (rustc, cargo) - [Install Rust](https://www.rust-lang.org/tools/install).

#### TypeScript

```bash
# First time: Install dependencies
npm install

# Compile and run
tsc solution.ts --moduleResolution node --esModuleInterop --resolveJsonModule
node solution.js
```

**Alternative**: Use `ts-node` for direct execution:
```bash
# First time: Install dependency
npm install -g ts-node

# Run
ts-node solution.ts
```

**Requirements**: Node.js and TypeScript (configured via `package.json`).

### Running with Test Input

All solutions can use test input files (`test_1.txt`, `test_2.txt`, etc.) by modifying the file path in the solution's I/O code. Test files are committed to git (unlike `input.txt` which is gitignored).

Example in Python:
```python
# For main input
with open("../data/input.txt", 'r') as f:
    data = f.read()

# For test input
with open("../data/test_1.txt", 'r') as f:
    data = f.read()
```

## Spec-Driven Development Workflow

The repository follows a Spec-Driven Development (SDD) workflow using spec-kit:

### Full Workflow (via `/day` command)

1. **Get Problem Description** - Fetches AOC problem description and populates README.md
2. **Get Input** - Downloads input for the day from AOC
3. **Create Specifications** - Generates spec-kit artifacts:
   - `days/day_NN/specs/day_NN_spec.md` - Specification
   - `days/day_NN/specs/day_NN_plan.md` - Technical plan
   - `days/day_NN/specs/day_NN_tasks.md` - Task breakdown
4. **Generate Solutions** - Creates code in all 12 languages
5. **Run Solutions** - Executes solutions, extracts answers, updates README
6. **Submit Answers** - Submits Part 1 and Part 2 answers to AOC
7. **Write Analysis** - Generates ANALYSIS.md comparing implementations
8. **Version Control** - Commits and pushes to GitHub

### Manual Workflow Steps

You can also run steps individually:

1. `/get_description <NN>` - Fetch and populate README.md with problem description
2. `/get_input <NN>` - Download input file for the day
3. `/create_specs <NN>` - Generate spec-kit artifacts for the day
4. `/generate_solutions <NN>` - Generate solution code in all 12 languages
5. `/run_solutions <NN>` - Run solutions, get answers, update README
6. `/submit_answer <NN> <answer> [part]` - Submit answer to AOC
7. `/write_analysis <NN>` - Generate ANALYSIS.md
8. `/push_to_gh` - Push code to GitHub

### For Repository Users

If you've pulled the repo with existing solutions:

- `/quick_solve <NN>` - Quick workflow:
  1. Get your own input file
  2. Run existing solutions
  3. Submit your answers
  4. Push to your fork/branch

## Project Guidelines

### Code Style

- **Idiomatic Code**: All code should be written idiomatically for each language
- **Efficiency First**: Always generate efficient code from the start (Part 2 typically requires it)
- **Minimal Dependencies**: Use as few external libraries as possible (prefer standard library)
- See `.cursor/rules/code_style.md` for detailed guidelines

### File I/O

- **Perform file I/O inline** within solution files
- Solutions should be self-contained with no external dependencies
- Keep algorithm-specific code in solution files

### Input Files

- `input.txt` - Official input (gitignored, user-specific)
- `test_1.txt`, `test_2.txt`, etc. - Test inputs (committed to git)

See `.cursor/REPO_STRUCTURE.md` for comprehensive repository structure documentation.
