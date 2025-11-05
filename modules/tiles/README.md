# Tiles - Tiling Window Manager for Emacs

A tiling window manager for Emacs inspired by i3wm. Automatically manages window layouts with configurable tiling schemes.

## Features

- **Multiple Layout Modes**: Horizontal, vertical, grid, and master-stack layouts
- **Automatic Layout Management**: Windows are automatically arranged based on the active layout
- **Buffer Ordering**: Maintains an ordered list of buffers for consistent display
- **Auto-balancing**: Optionally balance window sizes automatically
- **Easy Switching**: Cycle through layouts with a single command

## Layout Modes

### Horizontal
Splits windows horizontally (left to right). Good for wide monitors and side-by-side comparisons.

### Vertical
Splits windows vertically (top to bottom). Good for tall monitors and sequential workflows.

### Grid
Arranges windows in a grid pattern. Automatically calculates optimal rows and columns.

### Master-Stack
One master window on the left, remaining windows stacked vertically on the right. Inspired by dwm/xmonad.

## Installation

The module is already configured in `modules/ui.el`:

```elisp
(use-package tiles
  :straight nil
  :load-path "modules/tiles"
  :commands (tiles-mode tiles-setup tiles-refresh)
  :custom
  (tiles-default-layout 'horizontal)
  (tiles-master-window-ratio 0.5)
  (tiles-auto-balance t))
```

## Usage

### Enable Tiles Mode

```
SPC wts    # Setup tiles with current buffers
SPC wtt    # Toggle tiles mode on/off
```

### Layout Commands

```
SPC wtl    # Set specific layout (prompts for choice)
SPC wtc    # Cycle through all layouts
SPC wtr    # Refresh current layout
```

### Navigation

```
SPC wtn    # Focus next window
SPC wtp    # Focus previous window
```

## Keybindings

All tiles keybindings are under `SPC wt` (window → tiles):

| Key         | Command                | Description                |
|-------------|------------------------|----------------------------|
| `SPC wtt`   | tiles-mode             | Toggle tiles mode          |
| `SPC wts`   | tiles-setup            | Initialize with buffers    |
| `SPC wtr`   | tiles-refresh          | Refresh layout             |
| `SPC wtl`   | tiles-set-layout       | Choose layout              |
| `SPC wtc`   | tiles-cycle-layout     | Cycle through layouts      |
| `SPC wtn`   | tiles-focus-next       | Focus next window          |
| `SPC wtp`   | tiles-focus-previous   | Focus previous window      |

## Configuration

### Default Layout

Set the default layout when tiles mode is activated:

```elisp
(setq tiles-default-layout 'horizontal)  ; or 'vertical, 'grid, 'master-stack
```

### Master Window Ratio

For master-stack layout, set the ratio of space for the master window:

```elisp
(setq tiles-master-window-ratio 0.5)  ; 50% for master, 50% for stack
```

### Auto-balance

Enable or disable automatic window balancing:

```elisp
(setq tiles-auto-balance t)  ; t or nil
```

## How It Works

1. **Buffer List Management**: Tiles maintains an ordered list of buffers to display
2. **Layout Application**: When the layout is refreshed, tiles arranges windows according to the active layout mode
3. **Automatic Filtering**: Only displays "normal" buffers (filters out special buffers like minibuffer)

## Examples

### Horizontal Layout (3 buffers)
```
┌─────────┬─────────┬─────────┐
│ Buffer1 │ Buffer2 │ Buffer3 │
│         │         │         │
└─────────┴─────────┴─────────┘
```

### Vertical Layout (3 buffers)
```
┌───────────────────┐
│     Buffer1       │
├───────────────────┤
│     Buffer2       │
├───────────────────┤
│     Buffer3       │
└───────────────────┘
```

### Master-Stack Layout (4 buffers)
```
┌─────────┬─────────┐
│         │ Buffer2 │
│ Buffer1 ├─────────┤
│ (master)│ Buffer3 │
│         ├─────────┤
│         │ Buffer4 │
└─────────┴─────────┘
```

## License

This module is part of the edmacs configuration and is licensed under the GNU General Public License v3.0 or later.
