# Code Style Rules

## Idiomatic Code

All code should be written in a way as idiomatic to its language as possible. Each language has its own conventions, patterns, and best practices that should be followed.

## Language-Specific Idioms

### General Principles

- Use language-specific data structures and patterns
- Follow the language's standard library conventions
- Leverage language features that make code more expressive
- Avoid anti-patterns specific to each language

### Examples

- **C**: Manual memory management, pointer arithmetic, struct-based organization
- **Python**: List comprehensions, generator expressions, context managers
- **TypeScript**: Strong typing, async/await, functional programming with map/filter/reduce
- **Java**: Object-oriented design, interfaces, streams API
- **Rust**: Ownership, borrowing, pattern matching, Result/Option types
- **Kotlin**: Null safety, extension functions, data classes
- **Haskell**: Purely functional style, pattern matching, type classes
- **Go**: Interfaces, goroutines/channels, error handling conventions
- **Ruby**: Blocks, method chaining, metaprogramming where appropriate
- **Clojure**: Immutable data structures, lazy sequences, higher-order functions
- **Elixir**: Pattern matching, pipe operator, process-based concurrency
- **Julia**: Multiple dispatch, array operations, type system

## Code Organization

- Keep functions focused and single-purpose
- Use meaningful variable and function names
- Document complex algorithms and non-obvious logic
- Follow each language's commenting conventions

## Error Handling

- Handle errors appropriately for each language:
  - C: Return codes and errno checking
  - Python: Exceptions with try/except
  - TypeScript: Promise rejection handling
  - Java: Exception handling with throws
  - Rust: Result and Option types
  - Go: Error return values
  - Others: Language-appropriate error handling

## Performance Considerations

While prioritizing idiomatic code, also consider:
- Time complexity of algorithms
- Space complexity
- Language-specific performance characteristics
- Avoid premature optimization
