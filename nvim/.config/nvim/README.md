# RubyVim

## Overview

This is my Neovim configuration using lazy.nvim as the plugin manager. The configuration follows a modular structure with separate files for core settings, utilities, and individual plugin configurations.

## Development Commands

### Formatting

```bash
# Format Lua files with stylua (configured for 4 spaces)
stylua .
```

### Plugin Management

- Open Neovim and run `:Lazy` to manage plugins
- `:Lazy sync` - Install/update plugins
- `:Lazy clean` - Remove unused plugins
- Plugin versions are locked in `lazy-lock.json`

### LSP and Tooling

- `:Mason` - Manage LSP servers and tools (press `g?` for help)
- `:LspInfo` - View LSP server status
- `:ConformInfo` - View formatter configuration

## Architecture

### Entry Point

`init.lua` loads modules in this order:

1. `startupSettings` - Leader keys, autocommands, Wezterm integration
2. `options` - Vim options
3. `lazy` - Plugin manager bootstrap and setup
4. `dailyColorscheme` - Automatically rotates colorscheme daily
5. `utils` - Keymap helper functions
6. `keybindings` - Global keymaps

### Directory Structure

```
lua/adudenamedruby/
├── core/                    # Core configuration modules
│   ├── startupSettings.lua  # Leader keys, global settings, autocommands
│   ├── options.lua          # Vim options (indent, UI, behavior)
│   ├── dailyColorscheme.lua # Daily colorscheme rotation system
│   ├── keybindings.lua      # Global keybindings
│   └── utils.lua            # Keymap helper functions
├── lazy.lua                 # Plugin manager bootstrap
└── plugins/                 # Individual plugin configurations (lazy.nvim style)
    ├── nvim-lspconfig.lua   # LSP configuration
    ├── blink.lua            # Completion engine
    ├── conform.lua          # Formatting
    ├── harpoon.lua          # File navigation
    ├── fzf-lua.lua          # Fuzzy finder
    └── ...                  # 40+ other plugin configs
```

### Core Concepts

#### Leader Keys

- `<space>` is the main leader key
- `,` is the local leader key

#### Plugin System

All plugin configurations live in `lua/adudenamedruby/plugins/` as separate files. They return lazy.nvim plugin specs and are automatically loaded by `require("lazy").setup({ import = "adudenamedruby.plugins" })`.

#### Keymap Utilities

The `utils.lua` module provides helper functions for creating keymaps:

- `GMap(keys, func, desc, mode, expr)` - Global keymap
- `GLMap(keys, func, desc, mode, expr)` - Global leader keymap (prepends `<leader>`)
- `PMap(keys, func, desc, mode, expr)` - Plugin keymap (returns lazy.nvim keys spec)
- `PLMap(keys, func, desc, mode, expr)` - Plugin leader keymap (returns lazy.nvim keys spec with `<leader>`)

Most plugins define their keymaps using `PLMap` in the `keys` function, which enables lazy-loading.

#### Daily Colorscheme Rotation

The config automatically rotates between colorschemes daily:

- State persisted in `~/.cache/nvim/daily_colorscheme`
- Available schemes: catppuccin, tokyonight-moon, kanagawa, carbonfox, duskfox
- To manually change: edit the cache file or restart Neovim the next day

#### LSP Configuration

LSP setup uses Mason for automatic installation:

- Configured servers: lua_ls, pylsp, clojure_lsp, sourcekit (Swift), cssls, dockerls, html, jsonls, yamlls
- Auto-installs formatters: ruff, prettier, shfmt, stylua, codelldb
- LSP keymaps are created on `LspAttach` and use fzf-lua for fuzzy selection
- Sourcekit (Swift) has special handling for utf-8 offset encoding

#### Formatting

Conform.nvim handles formatting with format-on-save enabled:

- Uses language-specific formatters (stylua, prettier, rustfmt, shfmt)
- Format-on-save disabled for C/C++ (no standardized style)
- Falls back to LSP formatting when no formatter available
- Manual format: `<leader>uf`

#### Wezterm Integration

The config sets user variables to tell Wezterm when Neovim is active (`IS_NVIM` user var). This enables seamless navigation between Wezterm panes and Neovim splits.

## Key Patterns

### Adding a New Plugin

1. Create `lua/adudenamedruby/plugins/plugin-name.lua`
2. Return a lazy.nvim plugin spec table
3. Use `PLMap()` for leader-prefixed keymaps that enable lazy-loading
4. Add which-key groups in the config function if needed

### Modifying Keybindings

- Global keybindings: Edit `lua/adudenamedruby/core/keybindings.lua`
- Plugin-specific keybindings: Edit the plugin's file in `lua/adudenamedruby/plugins/`
- Use the utility functions from `utils.lua` for consistency

### Testing Changes

1. Restart Neovim or use `:Lazy reload {plugin-name}`
2. For core changes, restart Neovim entirely
3. Check for errors with `:messages`
