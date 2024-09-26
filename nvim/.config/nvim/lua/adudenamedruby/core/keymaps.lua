-- Keymaps
--
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Function: KMap
-- Description: Binds a specified keymap to an action with noremap and silent mode
-- Parameters:
--   mode - The modes in which you want the keybind to appear in. Default value = "n"
--   keys - The keys for the keybind
--   func - What to do when pressing that keybind
--   desc - A description to show up in which-key. Default value = ""
KMap = function(mode, keys, func, desc)
	mode = mode or "n"
	desc = desc or ""
	vim.keymap.set(mode, keys, func, { desc = desc, noremap = true, silent = true })
end

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
--close NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
KMap("t", "<Esc><Esc>", "<C-\\><C-n>", "exit terminal mode")

-- Resize with arrows
KMap("n", "<Up>", ":resize -2<CR>")
KMap("n", "<Down>", ":resize +2<CR>")
KMap("n", "<Left>", ":vertical resize -2<CR>")
KMap("n", "<Right>", ":vertical resize +2<CR>")

-- Buffer menu
KMap("n", "<leader><TAB>", "<cmd>:b#<CR>", "switch to last buffer")
KMap("n", "<leader>be", "<cmd>:enew<CR>", "open empty buffer")
KMap("n", "<leader>bd", "<cmd>:bd<CR>", "delete buffer")
KMap("n", "<leader>bD", "<cmd>:bd!<CR>", "force delete buffer")
KMap("n", "<leader>bn", "<cmd>:bn<CR>", "next buffer")
KMap("n", "<leader>bp", "<cmd>:bp<CR>", "previous buffer")
KMap("n", "<leader>br", ":e<CR>:bd#<CR>:e<CR>", "reload buffer with file")

-- Error menu
KMap("n", "<leader>en", "<cmd>silent cc | silent cn<cr>zz", "jump to next issue")
KMap("n", "<leader>ep", "<cmd>silent cc | silent cp<cr>zz", "jump to previous issue")

-- Files menu
KMap("n", "<leader>fs", "<cmd>w<CR>", "save file")

-- Git menu
KMap("n", "<leader>gb", "<cmd>Gitsigns blame<CR>", "git blame")
KMap("n", "<leader>gl", "<cmd>Gitsigns blame_line<CR>", "git blame line")

-- Debug menu
KMap("n", "<leader>dp", vim.diagnostic.goto_prev, "go to previous diagnostic")
KMap("n", "<leader>dn", vim.diagnostic.goto_next, "go to next diagnostic")
KMap("n", "<leader>dl", vim.diagnostic.open_float, "show line diagnostics")
KMap("n", "<leader>dq", vim.diagnostic.setloclist, "open diagnostic quickfix list")

-- Help menu
KMap("n", "<leader>Hm", ":redir @a<CR>:messages<CR>:redir END<CR>:new<CR>:put a<CR>", "messages buffer")

KMap("n", "<leader>HM", "<cmd>Mason<CR>", "open Mason")

-- Quit menu
KMap("n", "<leader>qq", "<cmd>q<CR>", "quit nVim")
KMap("n", "<leader>qQ", "<cmd>q!<CR>", "force quit nVim")

-- Search menu
-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
KMap("n", "<leader>sc", "<cmd>nohlsearch<CR>", "clear search highlights")

-- Toggle menu
KMap("n", "<leader>tw", "<cmd>set wrap!<CR>", "toggle line wrapping")

-- Windows menu
--  See `:help wincmd` for a list of all window commands
KMap("n", "<leader>wh", "<C-w><C-h>", "move focus to the left window")
KMap("n", "<leader>wl", "<C-w><C-l>", "move focus to the right window")
KMap("n", "<leader>wj", "<C-w><C-j>", "move focus to the lower window")
KMap("n", "<leader>wk", "<C-w><C-k>", "move focus to the upper window")

KMap("n", "<leader>ws", "<cmd>:split<CR>", "horizontal split")
KMap("n", "<leader>wv", "<cmd>:vsplit<CR>", "vertical split")

KMap("n", "<leader>wd", "<cmd>:q<CR>", "delete current window")
KMap("n", "<leader>wo", "<cmd>:only<CR>", "close all windows except current")

function ToggleSplits()
	-- Get the current window layout (width and height)
	local width = vim.api.nvim_win_get_width(0)
	local height = vim.api.nvim_win_get_height(0)

	-- If the current window is wider than it is tall, it's a horizontal split
	if width > height then
		-- Convert horizontal splits to vertical by using :wincmd L (Move to the left)
		vim.cmd("wincmd H")
	else
		-- Convert vertical splits to horizontal by using :wincmd H (Move down)
		vim.cmd("wincmd J")
	end
end

-- Bind the function to a key, e.g., <leader>t
KMap("n", "<leader>wc", ":lua ToggleSplits()<CR>", "change orientation")

-- QoL keymaps
-- vertical scroll & center
KMap("n", "<C-d>", "<C-d>zz")
KMap("n", "<C-u>", "<C-u>zz")

-- search & center
KMap("n", "n", "nzzzv")
KMap("n", "N", "Nzzzv")

-- don't yank x or visual paste
KMap("n", "x", '"_x')
KMap("v", "p", '"_dP')

-- Stay in indent mode
KMap("v", "<", "<gv")
KMap("v", ">", ">gv")
