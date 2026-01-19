# Language-Specific Rules

Detailed rules and guidelines for each programming language used in this project.

## C

### Memory Management
- Always check return values from memory allocation functions
- Free all allocated memory before program exit
- Use `valgrind` or similar tools to check for memory leaks

### File I/O
- Always check if file opening succeeded (return value is not NULL)
- Use `fgets` with appropriate buffer sizes for line reading
- Close files explicitly with `fclose`

### Error Handling
- Check return values from all system calls and library functions
- Use `errno` for error diagnosis when appropriate
- Return meaningful exit codes (0 for success, non-zero for errors)

### Code Structure
- Declare functions before use or use forward declarations
- Use const where appropriate for read-only data
- Prefer `size_t` for sizes and array indices

## Clojure

### Immutability
- Use immutable data structures
- Prefer functional transformations over mutation
- Use atoms, refs, or agents only when state is necessary

### Functional Programming
- Use higher-order functions (map, filter, reduce)
- Leverage lazy sequences for large datasets
- Use destructuring for function parameters

## Elixir

### Pattern Matching
- Use pattern matching extensively in function heads
- Use pattern matching in `case`, `cond`, and `with` expressions
- Destructure data structures with pattern matching

### Processes
- Use processes for concurrency and isolation
- Use message passing for process communication
- Consider OTP behaviors (GenServer, Supervisor) for complex processes

## Go

### Error Handling
- Always check errors explicitly
- Return errors as the last return value
- Wrap errors with context using `fmt.Errorf` or `errors.Wrap`

### Concurrency
- Use goroutines for concurrent operations
- Use channels for communication between goroutines
- Understand when to use sync primitives (mutexes, wait groups)

### Interfaces
- Define small, focused interfaces
- Use interfaces for abstraction and testing
- Follow the "accept interfaces, return structs" pattern

## Haskell

### Functional Style
- Write pure functions when possible
- Use pattern matching extensively
- Leverage higher-order functions (map, fold, filter)

### Type System
- Define types for domain concepts
- Use type synonyms and newtypes appropriately
- Make use of type classes (Show, Read, Eq, Ord, etc.)

### Lazy Evaluation
- Understand lazy evaluation implications
- Use strict evaluation (`seq`, `$!`) when needed
- Consider space leaks with lazy lists

## Java

### Object-Oriented Design
- Use classes and interfaces appropriately
- Follow single responsibility principle
- Use access modifiers correctly (private, protected, public)

### Collections
- Prefer modern Java collections (ArrayList, HashMap, etc.)
- Use streams API for functional-style operations
- Use `Optional` to avoid null pointer exceptions

### Exception Handling
- Catch specific exceptions, not generic Exception
- Document checked exceptions in method signatures
- Use try-with-resources for resource management

## Julia

### Multiple Dispatch
- Define methods with appropriate type signatures
- Use abstract types for interfaces
- Leverage Julia's type system for performance

### Array Operations
- Use vectorized operations when appropriate
- Prefer broadcasting (`f.(x)`) for element-wise operations
- Consider memory layout for performance

## Perl

### Text Processing
- Leverage Perl's powerful regular expressions for parsing
- Use hash-based data structures for key-value operations
- Follow Perl best practices for file I/O and string manipulation

### Style
- Use meaningful variable names with appropriate sigils ($, @, %)
- Prefer built-in functions and operators over complex constructs
- Use `strict` and `warnings` pragmas for better code quality

## Python

### Type Hints
- Use type hints for all function signatures
- Use `typing` module for complex types (List, Dict, Tuple, Optional, etc.)
- Document return types explicitly

### Style
- Follow PEP 8 style guide
- Use list comprehensions and generator expressions where appropriate
- Prefer `pathlib.Path` over string paths for file operations

### Best Practices
- Use context managers (`with` statements) for file I/O and resources
- Prefer `enumerate()` over range(len()) for iteration with indices
- Use `argparse` or similar for command-line arguments if needed

## Ruby

### Ruby Idioms
- Use blocks and procs for iteration and callbacks
- Leverage Ruby's metaprogramming features when appropriate
- Use symbols for keys and identifiers

### Style
- Follow Ruby style guide conventions
- Use meaningful method names with question marks for predicates
- Prefer `map`, `select`, `reduce` over explicit loops

## Rust

### Ownership and Borrowing
- Understand ownership rules and apply them correctly
- Use references (`&`) when borrowing is sufficient
- Use `Box`, `Rc`, or `Arc` only when necessary

### Error Handling
- Prefer `Result<T, E>` over panicking
- Use `?` operator for error propagation
- Use `Option<T>` for nullable values

### Pattern Matching
- Use `match` for exhaustive pattern matching
- Use `if let` for single pattern matches
- Use destructuring extensively

## TypeScript

### Type Safety
- Avoid `any` type; use `unknown` if type is truly unknown
- Use strict mode (`"strict": true` in tsconfig.json)
- Leverage union types and discriminated unions

### Modern Features
- Use async/await instead of promise chains
- Use destructuring and spread operators
- Use optional chaining (`?.`) and nullish coalescing (`??`)

### Node.js Specifics
- Use `fs/promises` for async file operations
- Handle errors with try/catch for async operations
- Use `path.join()` or `path.resolve()` for file paths
