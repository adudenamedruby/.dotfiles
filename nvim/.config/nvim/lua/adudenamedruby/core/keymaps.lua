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
KMap = function(keys, func, desc, mode, expr)
    mode = mode or "n"
    desc = desc or ""
    expr = expr or false

    if expr then
        vim.keymap.set(mode, keys, func, { expr = true, desc = desc, noremap = true, silent = true })
    else
        vim.keymap.set(mode, keys, func, { desc = desc, noremap = true, silent = true })
    end
end

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
--close NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
KMap("<Esc><Esc>", "<C-\\><C-n>", "exit terminal mode", "t")

KMap("j", "gj")
KMap("k", "gk")

-- Buffer menu
KMap("<leader><TAB>", "<cmd>:b#<CR>", "switch to last buffer")
KMap("<leader>be", "<cmd>:enew<CR>", "open empty buffer")
KMap("<leader>bd", "<cmd>:bd<CR>", "delete buffer")
KMap("<leader>bD", "<cmd>:bd!<CR>", "force delete buffer")
KMap("<leader>bo", "<cmd>:bd!# | e#<CR>", "force delete buffer")
KMap("<leader>bn", "<cmd>:bn<CR>", "next buffer")
KMap("<leader>bp", "<cmd>:bp<CR>", "previous buffer")
KMap("<leader>br", ":e<CR>:bd#<CR>:e<CR>", "reload buffer with file")

-- Error menu
KMap("<leader>en", "<cmd>silent cc | silent cn<cr>zz", "jump to next issue")
KMap("<leader>ep", "<cmd>silent cc | silent cp<cr>zz", "jump to previous issue")

-- Files menu
KMap("<leader>fs", "<cmd>w<CR>", "save file")

-- Git menu
KMap("<leader>gb", "<cmd>Gitsigns blame<CR>", "git blame")
KMap("<leader>gl", "<cmd>Gitsigns blame_line<CR>", "git blame line")

-- Debug menu
KMap("<leader>dp", vim.diagnostic.goto_prev, "go to previous diagnostic")
KMap("<leader>dn", vim.diagnostic.goto_next, "go to next diagnostic")
KMap("<leader>dl", vim.diagnostic.open_float, "show line diagnostics")
KMap("<leader>dq", vim.diagnostic.setloclist, "open diagnostic quickfix list")

-- Help menu
KMap("<leader>Hm", ":redir @a<CR>:messages<CR>:redir END<CR>:new<CR>:put a<CR>", "messages buffer")

KMap("<leader>HM", "<cmd>Mason<CR>", "open Mason")

-- quickfix menu
KMap("<leader>xn", "<cmd>cnext<CR>zz", "quickfix list next")
KMap("<leader>xp", "<cmd>cprev<CR>zz", "quickfix list previous")
KMap("<leader>xN", "<cmd>lnext<CR>zz", "loclist next")
KMap("<leader>xP", "<cmd>lprev<CR>zz", "loclist previous")
KMap("<leader>xo", "<cmd>copen<CR>", "open quickfix list")
KMap("<leader>xc", "<cmd>cclose<CR>", "close quickfix list")
KMap("<leader>xO", "<cmd>lopen<CR>", "open loclist")
KMap("<leader>xC", "<cmd>lclose<CR>", "close loclist")

-- Quit menu
KMap("<leader>qq", "<cmd>qa!<CR>", "quit nVim")

-- Search menu
-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
KMap("<leader>sc", "<cmd>nohlsearch<CR>", "clear search highlights")

-- Toggle menu
KMap("<leader>tw", "<cmd>set wrap!<CR>", "toggle line wrapping")

-- Windows menu
--  See `:help wincmd` for a list of all window commands
KMap("c-h", "<cmd>wincmd h<CR>", "move focus to the left window")
KMap("c-l", "<cmd>wincmd l<CR>", "move focus to the right window")
KMap("c-j", "<cmd>wincmd j<CR>", "move focus to the lower window")
KMap("c-k", "<cmd>wincmd k<CR>", "move focus to the upper window")

KMap("<leader>ws", "<cmd>:split<CR>", "horizontal split")
KMap("<leader>wv", "<cmd>:vsplit<CR>", "vertical split")

KMap("<leader>wd", "<cmd>:q<CR>", "delete current window")
KMap("<leader>wo", "<cmd>:only<CR>", "close all windows except current")

-- Resize with arrows
KMap("<C-Up>", ":resize +2<CR>")
KMap("<C-Down>", ":resize -2<CR>")
KMap("<C-Left>", ":vertical resize +2<CR>")
KMap("<C-Right>", ":vertical resize -2<CR>")

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
KMap("<leader>wc", ":lua ToggleSplits()<CR>", "change orientation")

-- QoL keymaps
-- vertical scroll & center
KMap("<C-d>", "<C-d>zz")
KMap("<C-u>", "<C-u>zz")

-- search & center
KMap("n", "nzzzv")
KMap("N", "Nzzzv")

-- don't yank x or visual paste
KMap("x", '"_x')
KMap("p", '"_dP', "", "v")

-- Stay in indent mode
KMap("<", "<gv", "", "v")
KMap(">", ">gv", "", "v")

-- I use <C-w> as my multiplexer key, so I want it do do nothing in nvim
-- vim.keymap.del({ "n", "i", "v" }, "<C-w>")

-- Move Lines
KMap("<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", "Move Down")
KMap("<A-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", "Move Up")
KMap("<A-j>", "<esc><cmd>m .+1<cr>==gi", "Move Down", "i")
KMap("<A-k>", "<esc><cmd>m .-2<cr>==gi", "Move Up", "i")
KMap("<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", "Move Down", "v")
KMap("<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", "Move Up", "v")

-- Saner behaviour of n and N
KMap("n", "'Nn'[v:searchforward].'zv'", "Next Search Result", "n", true)
KMap("n", "'Nn'[v:searchforward]", "Next Search Result", "x", true)
KMap("n", "'Nn'[v:searchforward]", "Next Search Result", "o", true)
KMap("N", "'nN'[v:searchforward].'zv'", "Prev Search Result", "n", true)
KMap("N", "'nN'[v:searchforward]", "Prev Search Result", "x", true)
KMap("N", "'nN'[v:searchforward]", "Prev Search Result", "o", true)
