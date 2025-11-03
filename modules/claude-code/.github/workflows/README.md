# GitHub Actions Workflows

This directory contains CI/CD workflows for the Claude Code REPL project.

## Workflows

### 1. CI (`ci.yml`)

**Comprehensive testing across multiple platforms and Emacs versions**

**Triggers:**
- Push to main/master/develop branches
- Pull requests
- Manual dispatch

**Jobs:**

#### Test Matrix
Tests against multiple configurations:
- **OS**: Ubuntu (primary), macOS
- **Emacs versions**: 28.1, 28.2, 29.1, 29.2, snapshot (experimental)
- **Total combinations**: 5+ configurations

**Steps:**
1. Setup Emacs for target version
2. Cache Eldev dependencies
3. Install Eldev
4. Install project dependencies
5. Run demo smoke test
6. Byte-compile with warnings as errors
7. Run test suite (if tests exist)
8. Run linter (non-blocking)

#### Build Job
Validates package can be built:
1. Byte-compile all files
2. Create package archive
3. Verify package metadata

#### Documentation Job
Checks documentation quality:
1. Verify README exists
2. Run markdown linter
3. Check for common issues

#### Report Job
Aggregates results from all jobs and reports final status.

**When to use:** This is the main CI workflow. Runs automatically on pushes and PRs.

---

### 2. Checks (`checks.yml`)

**Quick validation workflow**

**Triggers:**
- Push to main/master
- Pull requests

**What it does:**
Runs all checks in sequence on latest stable Emacs:
1. Prepare dependencies
2. Run demo
3. Compile
4. Test
5. Lint

**When to use:** Simpler, faster checks for rapid feedback. Good for development workflow.

---

### 3. Badge (`badge.yml`)

**Generate status badge**

**Triggers:**
- Push to main/master
- Weekly schedule (Sunday)

**What it does:**
1. Runs basic checks
2. Generates dynamic badge
3. Updates Gist with status

**Setup required:**
1. Create a GitHub Gist
2. Create personal access token with gist scope
3. Add as `GIST_SECRET` in repository secrets
4. Update `YOUR_GIST_ID_HERE` in workflow

**Badge URL:**
```markdown
![Build Status](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/USERNAME/GIST_ID/raw/claude-code-badge.json)
```

---

## Configuration

### Caching

All workflows cache Eldev dependencies to speed up builds:

```yaml
- uses: actions/cache@v4
  with:
    path: ~/.cache/eldev
    key: ${{ runner.os }}-eldev-${{ hashFiles('**/Eldev') }}
```

Cache is invalidated when `Eldev` file changes.

### Matrix Testing

The CI workflow uses a matrix strategy for comprehensive testing:

```yaml
strategy:
  matrix:
    os: [ubuntu-latest]
    emacs_version: ['28.1', '28.2', '29.1', '29.2']
    experimental: [false]
  include:
    - os: macos-latest
      emacs_version: '29.2'
    - os: ubuntu-latest
      emacs_version: 'snapshot'
      experimental: true
```

- `experimental: true` allows those builds to fail without blocking
- macOS testing for platform compatibility
- Snapshot testing for upcoming Emacs features

### Eldev Options

Workflows use these Eldev flags:
- `-d` - Debug mode (show backtraces)
- `-t` - Trace mode (verbose output)
- `-T` - Time commands
- `--warnings-as-errors` - Strict compilation

## Local Testing

You can simulate CI locally:

```bash
# Simulate the checks workflow
eldev prepare
eldev demo
eldev compile
eldev test
eldev lint

# Or use the custom check command
eldev check

# Simulate CI environment
CI=true eldev compile
```

## Adding New Workflows

To add a new workflow:

1. Create `new-workflow.yml` in this directory
2. Use this template:

```yaml
name: My Workflow

on:
  push:
    branches: [ main ]

jobs:
  my-job:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: purcell/setup-emacs@master
        with:
          version: '29.2'

      - name: Install Eldev
        run: |
          curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev > eldev
          chmod +x eldev
          sudo mv eldev /usr/local/bin/

      - name: My step
        run: eldev my-command
```

## Troubleshooting

### Workflow fails with "Eldev not found"

The Eldev installation step may have failed. Check:
1. Network connectivity in CI
2. Eldev repository availability
3. Script download URL is current

### Cache not working

Clear cache manually:
1. Go to Actions → Caches
2. Delete old caches
3. Re-run workflow

### Tests timeout

Increase timeout in workflow:

```yaml
- name: Run tests
  timeout-minutes: 10
  run: eldev test
```

### Matrix build fails on specific Emacs version

If a specific Emacs version consistently fails:
1. Mark it as experimental:
   ```yaml
   experimental: [false]
   # Then add to include section with experimental: true
   ```
2. Or remove it from the matrix

## Status Badges

Add these to your README.md:

```markdown
![CI](https://github.com/USERNAME/REPO/workflows/CI/badge.svg)
![Checks](https://github.com/USERNAME/REPO/workflows/Checks/badge.svg)
```

## Best Practices

1. **Fast feedback**: Use `checks.yml` for quick validation
2. **Comprehensive testing**: `ci.yml` runs on PRs for thorough checks
3. **Don't block on linting**: Use `continue-on-error` for linting
4. **Cache everything**: Cache Eldev and package dependencies
5. **Test multiple versions**: Ensure compatibility across Emacs versions
6. **Allow experimental builds to fail**: Use for snapshot/beta testing

## Security

- Never commit secrets to workflows
- Use GitHub Secrets for tokens
- Use `GITHUB_TOKEN` when possible (automatic)
- Limit workflow permissions to minimum required

## Performance Tips

1. **Use caching** - Saves ~30s per run
2. **Parallelize** - Matrix runs in parallel
3. **Fail fast** - Set `fail-fast: true` for faster feedback
4. **Skip unnecessary steps** - Use `if` conditions

## Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Eldev Documentation](https://emacs-eldev.github.io/eldev/)
- [setup-emacs Action](https://github.com/purcell/setup-emacs)
- [Cache Action](https://github.com/actions/cache)
