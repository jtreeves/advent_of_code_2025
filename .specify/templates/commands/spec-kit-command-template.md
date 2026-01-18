# Spec-Kit Command Template

This is a template for spec-kit commands that can be used with the spec-kit tool (`/speckit.<command>`).

## Command Structure

Spec-kit commands are invoked via `/speckit.<command>` and interact with the spec-kit tool to:
- Generate artifacts (specs, plans, tasks)
- Manage spec-kit workflow
- Integrate with the SDD (Spec-Driven Development) process

## Example Usage

```
/speckit.specify
/speckit.plan
/speckit.tasks
```

## Differences from Cursor Commands

**Spec-kit Commands** (`.specify/templates/commands/`):
- Used by the spec-kit tool itself
- Part of the Spec-Driven Development workflow
- Generate spec-kit artifacts (specs, plans, tasks)
- Work with spec-kit's AI integration
- Examples: `/speckit.specify`, `/speckit.plan`

**Cursor Commands** (`.cursor/commands/`):
- Used by Cursor IDE directly
- Define workflows that the AI assistant should follow
- Can invoke scripts, generate code, manage files
- User-facing, high-level operations
- Examples: `/day`, `/get_input`, `/run_solutions`

## When to Use

Use spec-kit command templates when:
- Creating new spec-kit workflow steps
- Defining how spec-kit should generate artifacts
- Integrating spec-kit with the project structure

Use Cursor commands when:
- Defining user-facing workflows
- Orchestrating multiple operations
- Managing files and scripts
