# Cursor Integration Template

This template shows how Cursor commands can integrate with spec-kit workflows.

## Integration Points

### From Cursor to Spec-Kit

Cursor commands can invoke spec-kit processes:
- `/create_specs` command calls spec-kit to generate specs
- Specs are generated in `days/day_NN/specs/` using spec-kit templates
- Cursor commands can reference spec-kit artifacts

### From Spec-Kit to Cursor

Spec-kit artifacts inform Cursor workflows:
- Specs define requirements that guide code generation
- Plans provide algorithms that solutions implement
- Tasks break down implementation steps

## Workflow Integration

```
1. User: /day 05
   ↓
2. Cursor Command: Fetches AOC problem, creates structure
   ↓
3. Cursor Command: /create_specs
   ↓
4. Spec-Kit: Generates specs using templates
   ↓
5. Cursor Command: /generate_solutions
   ↓
6. Cursor Command: Uses specs to guide code generation
```

## Command Template Structure

When creating new commands that integrate spec-kit and Cursor:

```markdown
# Command Name

## Purpose
[What this command does]

## Spec-Kit Integration
[How it uses spec-kit artifacts]

## Cursor Workflow
[What Cursor commands it calls or integrates with]

## Prerequisites
[What must exist before running]
```

## Key Differences

**Spec-Kit Commands** (`/speckit.*`):
- Low-level, spec-kit tool operations
- Generate specific artifacts
- Used within spec-kit workflow

**Cursor Commands** (`/command_name`):
- High-level, user-facing workflows
- Orchestrate multiple operations
- Can use spec-kit artifacts but are not spec-kit commands

**Integration Templates** (this folder):
- Define how Cursor and spec-kit work together
- Provide patterns for cross-tool workflows
- Bridge the gap between tools
