-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Diagnostic keymaps
-- vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
--close NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "exit terminal mode" })

-- TIP: Disable arrow keys in normal mode
vim.keymap.set("n", "<left>", '<cmd>echo "Use h to move!!"<CR>')
vim.keymap.set("n", "<right>", '<cmd>echo "Use l to move!!"<CR>')
vim.keymap.set("n", "<up>", '<cmd>echo "Use k to move!!"<CR>')
vim.keymap.set("n", "<down>", '<cmd>echo "Use j to move!!"<CR>')

-- Buffer menu
vim.keymap.set("n", "<leader><TAB>", "<cmd>:b#<CR>", { desc = "switch to last buffer" })
vim.keymap.set("n", "<leader>be", "<cmd>:enew<CR>", { desc = "open empty buffer" })
vim.keymap.set("n", "<leader>bd", "<cmd>:bd<CR>", { desc = "delete buffer" })
vim.keymap.set("n", "<leader>bn", "<cmd>:bn<CR>", { desc = "next buffer" })
vim.keymap.set("n", "<leader>bp", "<cmd>:bp<CR>", { desc = "previous buffer" })

-- Debug menu

-- Error menu
vim.keymap.set("n", "<leader>en", "<cmd>silent cc | silent cn<cr>zz", { desc = "jump to next issue" })
vim.keymap.set("n", "<leader>ep", "<cmd>silent cc | silent cp<cr>zz", { desc = "jump to previous issue" })

-- Files menu
vim.keymap.set("n", "<leader>fs", "<cmd>w<CR>", { desc = "save file" })

-- Git menu
vim.keymap.set("n", "<leader>gb", "<cmd>Gitsigns blame<CR>", { desc = "git blame" })
vim.keymap.set("n", "<leader>gc", "<cmd>Gitsigns blame_line<CR>", { desc = "git blame line" })

-- Help menu

-- Quit menu
vim.keymap.set("n", "<leader>qq", "<cmd>q<CR>", { desc = "[Q]uit nVim" })

-- Registers menu

-- Search menu
-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
vim.keymap.set("n", "<leader>sc", "<cmd>nohlsearch<CR>")

-- Toggle menu

-- Windows menu

-- Keybinds to make split navigation easier.
--  Use CTRL+<hjkl> to switch between windows

--  See `:help wincmd` for a list of all window commands
vim.keymap.set("n", "<leader>wh", "<C-w><C-h>", { desc = "move focus to the left window" })
vim.keymap.set("n", "<leader>wl", "<C-w><C-l>", { desc = "move focus to the right window" })
vim.keymap.set("n", "<leader>wj", "<C-w><C-j>", { desc = "move focus to the lower window" })
vim.keymap.set("n", "<leader>wk", "<C-w><C-k>", { desc = "move focus to the upper window" })

vim.keymap.set("n", "<leader>ws", "<cmd>:split<CR>", { desc = "horizontal split" })
vim.keymap.set("n", "<leader>wv", "<cmd>:vsplit<CR>", { desc = "vertical split" })

vim.keymap.set("n", "<leader>wd", "<cmd>:q<CR>", { desc = "delete current window" })
vim.keymap.set("n", "<leader>wo", "<cmd>:only<CR>", { desc = "close all windows except current" })
