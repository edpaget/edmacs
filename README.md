# edmacs

A modern, modular Emacs configuration with evil-mode, version-locked packages, and AI assistant integration.

## Features

- **Evil Mode**: Vim emulation with extensive plugin support
- **Version Locking**: Reproducible package management with straight.el
- **Modular Design**: Clean separation of concerns across modules
- **Modern Completion**: Vertico + Corfu + Consult ecosystem
- **LSP Support**: Full IDE features with LSP-mode
- **AI Integration**: Claude AI assistant via claude-code-ide.el
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
mv ~/.emacs.d ~/.emacs.d.backup
mv ~/.emacs ~/.emacs.backup  # if you have one
```

### 2. Clone This Repository

```bash
git clone https://github.com/yourusername/edmacs.git ~/.emacs.d
```

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

To use the Claude AI assistant, set your API key:

```bash
export ANTHROPIC_API_KEY="your-api-key-here"
```

Add this to your `~/.bashrc`, `~/.zshrc`, or equivalent.

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
│   ├── programming.el     # LSP, Flycheck, Apheleia
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
| `SPC aa` | AI chat |
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

| Key | Action |
|-----|--------|
| `SPC w` | Window menu |
| `SPC ws` | Split below |
| `SPC wv` | Split right |
| `SPC wd` | Delete window |
| `C-h/j/k/l` | Navigate windows |

### Git

| Key | Action |
|-----|--------|
| `SPC g` | Git menu |
| `SPC gg` | Magit status |
| `SPC gc` | Commit |
| `SPC gp` | Push |
| `SPC gP` | Pull |
| `SPC gl` | Log |

### Code (LSP)

When in an LSP-enabled buffer:

| Key | Action |
|-----|--------|
| `SPC c` | Code menu |
| `SPC ca` | Code action |
| `SPC cr` | Rename |
| `SPC cf` | Format |
| `SPC cd` | Go to definition |
| `SPC cR` | Find references |

### AI Assistant

| Key | Action |
|-----|--------|
| `SPC a` | AI menu |
| `SPC aa` | Chat with Claude |
| `SPC ae` | Explain code |
| `SPC ar` | Refactor |
| `SPC ad` | Document |

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
2. Configure packages, LSP, formatters, etc.
3. The file will be loaded automatically when opening files of that type

Example structure:

```elisp
;;; python.el --- Python configuration -*- lexical-binding: t -*-

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred))

(use-package poetry
  :hook (python-mode . poetry-tracking-mode))
```

### Included Languages

- Clojure (example in `modules/languages/clojure.el`)

## Customization

### Personal Customizations

Create a `custom.el` file in `~/.emacs.d/` for personal customizations:

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

To change the theme, edit `modules/ui.el`:

```elisp
(setq catppuccin-flavor 'mocha)  ; Options: latte, frappe, macchiato, mocha
```

Or use a different theme entirely by replacing the catppuccin-theme use-package block.

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

### LSP Not Working

1. Install the language server for your language
2. Check LSP status: `M-x lsp-describe-session`
3. Restart LSP: `M-x lsp-workspace-restart`

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
- [LSP Mode](https://emacs-lsp.github.io/lsp-mode/)
- [Magit](https://magit.vc/)

## Acknowledgments

This configuration is built on the shoulders of giants. Thanks to all the Emacs package maintainers and the community!
