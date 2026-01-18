# /push_to_gh Command

Pushes code to GitHub repository.

## Usage

```
/push_to_gh [commit_message] [branch]
```

If no commit message is provided, uses a default message.
If no branch is provided, uses the current branch.

## Behavior

1. Stages all changes (excluding gitignored files like `input.txt`)
2. Creates a commit with the specified message
3. Pushes to the specified branch (or current branch)
4. Handles cases where:
   - Remote doesn't exist
   - Branch conflicts
   - No changes to commit

## Commit Message Convention

Suggested format:
```
Day NN: [brief description]

- Implemented solutions in all 12 languages
- Added analysis document
- Updated documentation
```

## Safety

- Does not force push
- Does not push to `main`/`master` without explicit branch specification
- Excludes input files (gitignored)

## Example

```
/push_to_gh "Day 05: Implemented all solutions"
```

This commits and pushes all changes for Day 05.
