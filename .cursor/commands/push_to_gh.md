# /push_to_gh Command

Pushes code to GitHub repository.

## Usage

```
/push_to_gh [commit_message] [branch]
```

If no commit message is provided, uses a default message based on the status of the workflow and the rules defined for commit messages.
If no branch is provided, uses the current branch.

## Behavior

1. Stages all changes (excluding gitignored files like `input.txt`)
2. Creates a commit with the specified message
3. Pushes to the specified branch (or current branch)
4. Handles cases where:
   - Remote doesn't exist
   - Branch conflicts
   - No changes to commit

## Example

```
/push_to_gh "Solve day 5, part 1"
```

This commits and pushes the solution for part 1 for Day 05.
