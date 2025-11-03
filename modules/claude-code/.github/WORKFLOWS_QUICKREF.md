# GitHub Actions Workflows - Quick Reference

## Available Workflows

### 🚀 CI - Comprehensive Testing
**File**: `ci.yml`
**Trigger**: Push, PR, Manual
**Matrix**: 5+ Emacs versions, 2 platforms
**Time**: ~5-8 minutes

**What it does**:
- ✅ Tests on Emacs 28.1, 28.2, 29.1, 29.2, snapshot
- ✅ Tests on Ubuntu and macOS
- ✅ Byte-compiles with strict warnings
- ✅ Runs tests
- ✅ Lints code
- ✅ Builds package
- ✅ Checks documentation

---

### ⚡ Checks - Quick Validation
**File**: `checks.yml`
**Trigger**: Push, PR
**Emacs**: 29.2
**Time**: ~2-3 minutes

**What it does**:
```bash
eldev prepare
eldev demo
eldev compile
eldev test
eldev lint
```

---

### 📛 Badge - Status Updates
**File**: `badge.yml`
**Trigger**: Push, Weekly
**Setup**: Requires Gist

**What it does**:
- Runs basic checks
- Updates status badge
- Posts to Gist

---

## Quick Commands

### Run Locally
```bash
# Simulate CI
CI=true eldev compile

# Run all checks
eldev prepare && eldev demo && eldev compile && eldev test && eldev lint

# Or use shortcut
eldev check
```

### View Workflows
```
https://github.com/USERNAME/REPO/actions
```

### Re-run Failed Workflow
1. Go to Actions tab
2. Click failed workflow
3. Click "Re-run jobs"

---

## Matrix Configuration

| Emacs | OS | Experimental |
|-------|----|--------------|
| 28.1 | Ubuntu | No |
| 28.2 | Ubuntu | No |
| 29.1 | Ubuntu | No |
| 29.2 | Ubuntu | No |
| 29.2 | macOS | No |
| snapshot | Ubuntu | Yes |

---

## Status Badges

Add to README.md:

```markdown
![CI](https://github.com/USERNAME/REPO/workflows/CI/badge.svg)
![Checks](https://github.com/USERNAME/REPO/workflows/Checks/badge.svg)
```

---

## Workflow Files Location

```
.github/
└── workflows/
    ├── ci.yml          # Main CI pipeline
    ├── checks.yml      # Quick checks
    ├── badge.yml       # Badge generator
    └── README.md       # Full documentation
```

---

## Common Issues

### ❌ "Eldev not found"
**Fix**: Re-run workflow (installation may have failed)

### ❌ Tests timeout
**Fix**: Add `timeout-minutes: 10` to test step

### ❌ Cache miss
**Fix**: Clear Actions cache and re-run

### ⚠️ Experimental build failed
**Expected**: Snapshot builds can fail

---

## Typical Run Output

```
✓ Checkout code
✓ Set up Emacs (29.2)
✓ Cache Eldev (hit)
✓ Install Eldev
✓ Install dependencies
  [1/2] Installing package 'markdown-mode'...
  [2/2] Installing package 'projectile'...
✓ Run demo smoke test
  Running claude-code demo...
  ✓ All modules loaded successfully
  ✓ All key functions defined
  ✓ Demo complete! Claude Code is ready.
✓ Byte-compile
  ELC      claude-code-process.el
  ELC      claude-code-buffer.el
  ELC      claude-code-core.el
✓ Run tests
✓ Run linter
```

---

## Performance

| Metric | Value |
|--------|-------|
| First run | ~4 min |
| Cached run | ~2 min |
| Matrix total | ~5-8 min |
| Checks only | ~2-3 min |

---

## Next Steps

1. Push workflows to GitHub
2. Watch first run
3. Add badges to README
4. Set up branch protection (optional)
5. Configure notifications (optional)

---

**Full Documentation**: See [.github/workflows/README.md](.github/workflows/README.md)
