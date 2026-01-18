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

## Performance and Efficiency Requirements

### Efficiency is a Priority

**Always generate as efficient code as possible from the start.**

1. **Part 2 Considerations**: Part 2 problems typically require efficient solutions
   - Design Part 1 with Part 2 efficiency in mind
   - Avoid approaches that will need to be completely rewritten for Part 2
   - Consider scalability and performance from the beginning
   - Choose algorithms and data structures that can scale

2. **Algorithm Selection**:
   - Prefer efficient algorithms (O(n log n), O(n), etc.) over brute force when possible
   - Consider time complexity from the start
   - Consider space complexity and memory usage
   - Use appropriate data structures (sets for lookups, heaps for priority queues, etc.)

3. **Avoid Rewriting Between Parts**:
   - Structure Part 1 code so it can be extended for Part 2
   - If Part 1 needs optimization, do it before starting Part 2
   - Refactor shared logic into reusable functions when needed

### Minimal External Dependencies

**Use as few external libraries as possible.**

1. **Prefer Standard Library**: Always prefer standard library functionality over external packages
   - Most problems can be solved with built-in data structures and algorithms
   - Use standard library collections, algorithms, and utilities

2. **When External Libraries are Acceptable**:
   - Only when standard library lacks critical functionality
   - Only when the library is essential and doesn't add unnecessary complexity
   - Document why an external library is necessary

3. **Avoid Adding Dependencies For**:
   - Simple operations that standard library can handle
   - Convenience utilities when standard library is sufficient
   - "Nice-to-have" features that aren't essential

## Performance Considerations

While prioritizing idiomatic code and efficiency, also consider:
- Time complexity of algorithms (prefer efficient from the start)
- Space complexity (avoid unnecessary memory usage)
- Language-specific performance characteristics
- Balance efficiency with readability (efficient code should still be clear)
- Avoid premature optimization, but don't write obviously inefficient code
