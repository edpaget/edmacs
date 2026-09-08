# edmacs

A modern, modular Emacs configuration with evil-mode, version-locked packages, and AI assistant integration.

## Features

- **Evil Mode**: Vim emulation with extensive plugin support
- **Version Locking**: Reproducible package management with straight.el
- **Modular Design**: Clean separation of concerns across modules
- **Modern Completion**: Vertico + Corfu + Consult ecosystem
- **LSP Support**: Full IDE features with the built-in eglot client
- **AI Integration**: the Claude CLI hosted in Emacs via claude-term (ghostel)
- **Beautiful UI**: Catppuccin theme with Nano Modeline
- **Org Mode**: Enhanced with org-roam, org-modern, and org-appear
- **Git Integration**: Magit, diff-hl, and git-timemachine

## Prerequisites

- Emacs 29.1 or later (recommended)
- Git
- Ripgrep (for fast searching)
- Language servers for your programming languages (optional)

## Installation

### 1. Backup Your Current Configuration

If you have an existing Emacs configuration, back it up first:

```bash
mv ~/.config/emacs ~/.config/emacs.backup
mv ~/.emacs.d ~/.emacs.d.backup   # if you have one
mv ~/.emacs ~/.emacs.backup       # if you have one
```

`~/.emacs.d` matters even if you only intend to use the XDG path. Emacs
"prefer[s] the XDG location only if the `.emacs.d` location does not
exist" (`startup.el`), so a leftover `~/.emacs.d` silently wins and
`~/.config/emacs` is never read.

### 2. Clone This Repository

This config lives at the XDG location, `~/.config/emacs`:

```bash
git clone https://github.com/yourusername/edmacs.git ~/.config/emacs
```

If you keep the checkout elsewhere — this machine keeps it at
`~/Projects/edmacs` — symlink it instead:

```bash
ln -s ~/Projects/edmacs ~/.config/emacs
```

Emacs 27+ honours `XDG_CONFIG_HOME`; if you set it, substitute
`$XDG_CONFIG_HOME/emacs` for `~/.config/emacs` throughout.

### 3. First Launch

Start Emacs. On first launch, straight.el will automatically:
- Download and install itself
- Install all configured packages
- Compile everything

This may take a few minutes. Be patient!

### 4. Install Icon Fonts

After the initial setup, run:

```
M-x nerd-icons-install-fonts
```

Then restart Emacs for icons to display properly.

### 5. Configure AI Assistant (Optional)

Claude sessions launch the `claude` CLI, so install and authenticate it
once (see the Claude Code docs); Emacs inherits that login and needs no
API key of its own. Permission prompts are raised by the CLI itself, so an
Emacs-hosted session asks for exactly what a terminal session asks for.

## Shell Environment Synchronization

This configuration automatically syncs environment variables from your shell to Emacs using `exec-path-from-shell`. This ensures that:

- **PATH is correct** - Emacs can find all your command-line tools
- **Language tools work** - Node, Python, Ruby, etc. use the right versions
- **Environment variables are available** - API keys, tool configurations, etc.

### How It Works

On macOS and Linux GUI environments, the configuration automatically:
1. Runs your login shell (`bash -l` or `zsh -l`)
2. Extracts environment variables: `PATH`, `MANPATH`, `ANTHROPIC_API_KEY`, etc.
3. Sets them in Emacs

### Customizing Synced Variables

To sync additional environment variables, edit `modules/core.el`:

```elisp
(setq exec-path-from-shell-variables
      '("PATH"
        "MANPATH"
        "ANTHROPIC_API_KEY"
        "YOUR_CUSTOM_VAR"))  ; Add your variables here
```

### Terminal Emacs

If you only use Emacs in the terminal (not GUI), you can disable this:

```elisp
;; In modules/core.el, comment out or remove the exec-path-from-shell block
```

## Directory Structure

```
edmacs/
├── early-init.el           # Performance optimizations
├── init.el                 # Main entry point
├── modules/                # Configuration modules
│   ├── core.el            # Basic settings
│   ├── evil-config.el     # Evil mode
│   ├── keybindings.el     # General.el keybindings
│   ├── ui.el              # Theme and appearance
│   ├── completion.el      # Vertico, Corfu, Consult
│   ├── programming.el     # eglot, flymake, Apheleia
│   ├── ai.el              # AI assistant
│   ├── org-config.el      # Org mode
│   ├── git.el             # Magit and git tools
│   └── languages/         # Language-specific configs
│       └── clojure.el     # Example: Clojure setup
├── straight/
│   └── versions/          # Package version lockfiles (committed)
└── README.md
```

## Keybindings

This configuration uses **SPC** (spacebar) as the leader key in normal/visual mode, and **C-SPC** in insert/emacs mode.

### Essential Keybindings

| Key | Action |
|-----|--------|
| `SPC SPC` | M-x (execute command) |
| `SPC ff` | Find file |
| `SPC fr` | Recent files |
| `SPC bb` | Switch buffer |
| `SPC bd` | Kill buffer |
| `SPC gg` | Magit status |
| `SPC ss` | Search buffer |
| `SPC sp` | Search project (ripgrep) |
| `SPC pf` | Find file in project |
| `SPC an` | New Claude session |
| `SPC oa` | Org agenda |
| `SPC hf` | Describe function |

### File Operations

| Key | Action |
|-----|--------|
| `SPC f` | File menu |
| `SPC ff` | Find file |
| `SPC fs` | Save file |
| `SPC fd` | Open dired |

### Buffer Operations

| Key | Action |
|-----|--------|
| `SPC b` | Buffer menu |
| `SPC bb` | Switch buffer |
| `SPC bd` | Kill buffer |
| `SPC bn/bp` | Next/previous buffer |

### Window Operations

`C-w` is the window prefix, active in **every** evil state — including
insert state and inside a Claude terminal pane — so moving between windows
never means leaving insert first. It carries evil's own window map plus a
tmux-flavored layer; `SPC w` reaches the same commands under the same
letters.

| Key | Action |
|-----|--------|
| `C-w h/j/k/l` | Navigate windows |
| `C-w \|` / `C-w -` | Split right / below |
| `C-w H/J/K/L` | Resize pane (left/down/up/right) |
| `C-w RET` | Promote pane to main |
| `C-w =` | Rebalance the center windows |
| `C-w x` | Close pane (keeps the buffer) |
| `C-w d` | Delete window, or demote from main |
| `C-w m` | Pop a buffer into main |
| `C-w [` / `C-w ]` | Previous / next stack pane |
| `C-w <` / `C-w >` | Narrow / widen the stack column |
| `C-w S` | Toggle side windows |
| `SPC w` | Window menu (same commands, leader-driven) |
| `SPC ws` | Split below |
| `SPC wv` | Split right |
| `SPC wd` | Delete window |
| `C-h/j/k/l` | Navigate windows |

Deliberately shadowed, following tmux rather than vim: `C-w -` (was
decrease-height) and `C-w H/J/K/L` (were move-window-far-\*) become split and
resize, `C-w x` (was exchange) closes the pane, and insert-state `C-w`
(delete-word-backward, and a terminal pane's own word-erase) gives up its key.

### Modeline

`nano-modeline` renders one `:eval` form and never consults
`global-mode-string`, so anything reporting through that channel is invisible
(this is why an LSP client's own modeline diagnostics segment shows
nothing). `modules/ui.el`'s
"Modeline content" section adds what is missing and filters what is noise:

- **Name filtering.** `edmacs-modeline-name-filters` is an alist of
  `(REGEXP . REPLACEMENT)` applied to the *displayed* buffer name only — the
  buffer keeps its real name, so `switch-to-buffer` and
  `claude-term--parse-buffer-name` are unaffected. First match wins; a filter
  that would empty the name is ignored. Out of the box:

  | Buffer | Shown as |
  |---|---|
  | `*claude-term:edmacs:review*` | `edmacs:review` |
  | `*magit-diff: edmacs*` | `edmacs diff` |
  | `*helpful variable: tab-width*` | `tab-width` |
  | `*cider-repl edmacs*` | `edmacs repl` |

- **Diagnostics.** `edmacs-modeline-diagnostics` reads flymake directly and
  shows `E3 W2` in the stock `error`/`warning` faces. Silent — not zero — when
  flymake is off, still checking, or clean, so it costs no width normally.

- **Terminal panes.** `ghostel-mode` has no line of its own upstream, so a
  Claude pane fell through to the default text line and showed a file buffer's
  furniture. It now gets `>_  edmacs:review` on the left and the working
  directory on the right — no read-write box, no cursor position.

### Quitting and Restarting

Emacs runs as a launchd daemon (`brew services start emacs-plus@31`) whose
plist sets `KeepAlive` unconditionally — launchd relaunches on *any* exit. So
"quit" cannot mean "end the process": that is a restart, whatever it is bound
to. The three intents get three commands (`modules/sessions.el`):

| Key | Action |
|-----|--------|
| `SPC qq` | Close the frame; the daemon and your session stay up. Last visible frame hides Emacs, so the Dock tile keeps owning it and a click brings the layout straight back. |
| `SPC qr` | Restart the daemon: save, exit, let launchd relaunch and the frameset restore. |
| `SPC qQ` | `brew services stop` — the only one that really quits. Confirms first; starting Emacs again needs a terminal. |

Outside a daemon each falls back to the stock behavior
(`save-buffers-kill-terminal`, `restart-emacs`).

### Git

| Key | Action |
|-----|--------|
| `SPC g` | Git menu |
| `SPC gg` | Magit status |
| `SPC gc` | Commit |
| `SPC gp` | Push |
| `SPC gP` | Pull |
| `SPC gl` | Log |

### Code

The eglot verbs need a language server attached; the flymake ones work in
any buffer flymake checks, server or no server.

| Key | Action |
|-----|--------|
| `SPC c` | Code menu |
| `SPC ca` | Code action |
| `SPC cr` | Rename |
| `SPC cf` | Format |
| `SPC cd` | Go to definition |
| `SPC cR` | Find references |
| `SPC ch` | Hover doc |
| `SPC cw` | Diagnostics (consult) |
| `SPC cW` | Project diagnostics |
| `SPC cxl` | List buffer diagnostics |
| `SPC cxn` | Next error |
| `SPC cxp` | Previous error |

### AI Assistant

Claude runs as the real interactive CLI inside a ghostel terminal buffer
(`modules/claude-term.el`), tracked by a session registry
(`modules/claude-term-registry.el`).

Session panes are **ordinary windows**, not side windows, so `C-w =`
rebalances them along with everything else, `other-window` cycles through
them, and `delete-other-windows` closes them — the session keeps running
either way, since only the window goes.

| Key | Action |
|-----|--------|
| `SPC a` | Claude menu |
| `SPC an` | New session |
| `SPC aj` | Jump to session |
| `SPC aL` | List sessions |
| `SPC aw` | Toggle session pane |
| `SPC aA` | Show all sessions |
| `SPC ax` | Kill session |
| `SPC aX` | Kill all sessions |
| `SPC ar` | Rename session |

## Package Management

This configuration uses straight.el for reproducible package management with version locking.

### Updating Packages

**Update a single package:**

```
M-x straight-pull-package RET package-name RET
```

**Update all packages:**

```
M-x straight-pull-all
```

**Lock current versions:**

After updating and testing, lock the versions:

```
M-x straight-freeze-versions
```

This creates/updates `straight/versions/default.el` with current package commits.

**Commit the lockfile:**

```bash
git add straight/versions/
git commit -m "Update package versions"
```

### Restoring from Lockfile

If you need to restore packages to locked versions:

```
M-x straight-thaw-versions
```

### Adding New Packages

1. Add a `use-package` declaration in the appropriate module file
2. Restart Emacs or evaluate the declaration
3. Test the package
4. Lock versions with `M-x straight-freeze-versions`
5. Commit the updated lockfile

## Language Support

Language-specific configurations are in `modules/languages/`.

### Adding a New Language

1. Create `modules/languages/LANGUAGE.el`
2. Configure packages, the language server, formatters, etc.
3. The file will be loaded automatically when opening files of that type

Example structure:

```elisp
;;; python.el --- Python configuration -*- lexical-binding: t -*-

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . eglot-ensure))

(use-package poetry
  :hook (python-mode . poetry-tracking-mode))
```

### Included Languages

- Clojure (example in `modules/languages/clojure.el`)

## Customization

### Personal Customizations

Create a `custom.el` file in `~/.config/emacs/` for personal customizations
(`init.el` sets `custom-file` from `user-emacs-directory`, so this follows
the config wherever it lives):

```elisp
;;; custom.el --- Personal customizations

;; Your personal settings here
(setq user-full-name "Your Name"
      user-mail-address "your@email.com")
```

This file is gitignored and won't be overwritten.

### Disabling Modules

To disable a module, comment out its load line in `init.el`:

```elisp
;; (load-module "ai")  ; Disable AI assistant
```

### Font Configuration

Edit `modules/ui.el` to change the font. The configuration tries several fonts in order of preference.

### Theme

edmacs uses modus-themes (bundled with Emacs since 28, so no package
install is required). To change the variant, edit `modules/ui.el`:

```elisp
(load-theme 'modus-vivendi :no-confirm)   ; dark (default)
;; (load-theme 'modus-operandi :no-confirm) ; light
```

modus-themes and its sibling ef-themes both apply fixed-pitch inheritance
correctly across markdown, org, and 50+ other faces, so proportional
prose (see `variable-pitch-mode` in `modules/ai.el`) keeps code, tables,
and indentation monospaced without any extra per-face patching.

## Troubleshooting

### Slow Startup

1. Check startup time: `M-x emacs-init-time`
2. Profile startup: Install and run `esup` package
3. Disable unused modules

### Package Issues

1. Rebuild a package: `M-x straight-rebuild-package`
2. Rebuild all: `M-x straight-rebuild-all`
3. Check for errors: `M-x straight-check-all`

### Icons Not Showing

Run `M-x nerd-icons-install-fonts` and restart Emacs.

### Language Server Not Working

1. Install the language server for your language
2. Check the exchange: `M-x eglot-events-buffer`
3. Restart the server: `M-x eglot-reconnect`

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## License

This configuration is licensed under the GNU General Public License v3.0 or later. See the LICENSE file for details.

## Resources

- [Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Evil Mode](https://github.com/emacs-evil/evil)
- [straight.el](https://github.com/radian-software/straight.el)
- [Eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html)
- [Magit](https://magit.vc/)

## Acknowledgments

This configuration is built on the shoulders of giants. Thanks to all the Emacs package maintainers and the community!
