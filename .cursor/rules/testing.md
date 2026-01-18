# Testing Rules

## Test File Requirements

### Test Data Files

- Test input files are named `test_1.txt`, `test_2.txt`, etc. (not `test_input.txt`)
- Test files are located in `days/day_NN/data/` directory
- Test files are **committed to git** (unlike `input.txt` which is gitignored)
- Each day should contain AT LEAST 5 test case files with known expected outputs (as specified in spec files)

### Testing Workflow

All language solutions **must** include testing using the designated test data files:

1. **Test Against Test Files First**: 
   - Solutions must be tested against all test data files (`test_1.txt`, `test_2.txt`, etc.) before running on `input.txt`
   - Use utility functions (`get_test_input()`) to read test files
   - Solutions should return the expected result for each test case

2. **Validation Requirements**:
   - Each test case must produce the expected output specified in the spec file
   - Solutions should be refined until they pass all test cases
   - No solution should be considered complete without passing all test cases

3. **Final Verification**:
   - Only after passing all test cases should the solution be run against `input.txt`
   - This ensures correctness before submission to AOC

## Test Execution

### Language-Specific Testing

Each language should test solutions in an idiomatic way:
- **C**: Manual testing with known inputs/outputs, verify against expected values
- **Clojure**: Use `clojure.test` or simple assertions
- **Elixir**: Use ExUnit or simple pattern matching validation
- **Go**: Use standard testing patterns or simple validation
- **Haskell**: Property-based or example-based testing
- **Java**: JUnit or simple assertion checks
- **Julia**: Simple assertions or validation
- **Kotlin**: Simple assertions or validation checks
- **Python**: Use assertions or simple validation checks
- **Ruby**: Simple assertions or validation
- **Rust**: Use `assert!` macros for validation
- **TypeScript**: Type checking and runtime validation

### Testing Pattern

Solutions should follow this pattern:

```python
# Example pattern (language-specific syntax)
def solve(data):
    # Solution logic
    return result

# Test against test files
for test_num in range(1, 6):  # Assuming 5 test cases
    test_data = get_test_input(day, test_num)
    result = solve(test_data)
    # Validate result matches expected output from spec
    assert result == expected_outputs[test_num], f"Test {test_num} failed"

# Only after all tests pass, run on input.txt
final_data = get_input(day)
final_result = solve(final_data)
```

## Test Data Management

- Test cases are defined in spec files (`day_NN_spec.md`)
- Test input files are created manually or extracted from problem examples
- Each test file should ONLY contain raw data, formatted in a way that is exactly parallel to how that day's input file is formatted
- All documentation about the test files should be clearly documented with the expected output in that day's spec file
- Test files are shared across all implementations (committed to git)

## Principles

1. **Test-First Approach**: Always test against known test cases before running on full input
2. **Completeness**: All test cases must pass before considering a solution complete
3. **Consistency**: All 12 language implementations should produce the same results for test cases
4. **Clarity**: Test files should be self-contained with clear expected outputs

## Integration with Specs

- Spec files (`day_NN_spec.md`) should define at least 5 test cases with expected outputs
- Test files (`test_1.txt` through `test_N.txt`) should match the test cases defined in specs
- Solutions should reference spec files to understand expected outputs for test cases
