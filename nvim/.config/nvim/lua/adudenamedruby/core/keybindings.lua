-- Keymappings

-- local variables
local U = require("adudenamedruby.core.utils")

-- Buffer menu
U.GLMap("ba", "<cmd>:b#<CR>", "switch to last buffer")
U.GLMap("be", "<cmd>:enew<CR>", "open empty buffer")
U.GLMap("bd", "<cmd>:bd<CR>", "delete buffer")
U.GLMap("bD", "<cmd>:bd!<CR>", "force delete buffer")
U.GLMap("bo", "<cmd>:bd!# | e#<CR>", "force delete buffer")
U.GLMap("bn", "<cmd>:bn<CR>", "next buffer")
U.GLMap("bp", "<cmd>:bp<CR>", "previous buffer")
U.GLMap("br", ":e<CR>:bd#<CR>:e<CR>", "reload buffer with file")
U.GLMap("bs", "<cmd>ScratchVSplit<cr>", "scratch buffer (vertical)")
U.GLMap("bS", "<cmd>ScratchHSplit<cr>", "scratch buffer (horizontal)")

-- Debug menu
-- There are sereval keymaps in nvim-dap for the debugger
U.GLMap("dp", function()
    vim.diagnostic.jump({ count = -1, float = true })
end, "go to previous diagnostic")
U.GLMap("dn", function()
    vim.diagnostic.jump({ count = 1, float = true })
end, "go to next diagnostic")
U.GLMap("dl", vim.diagnostic.open_float, "show line diagnostics")
U.GLMap("dq", vim.diagnostic.setloclist, "open diagnostic quickfix list")

-- Files menu
U.GLMap("fs", "<cmd>w<CR>", "save file")
U.GLMap("ft", "<cmd>NvimTreeToggle<CR>", "file tree")
U.GLMap("fo", "<cmd>Oil --float<CR>", "oil")

-- Git menu
U.GLMap("gb", "<cmd>Gitsigns blame<CR>", "git blame")
U.GLMap("gl", "<cmd>Gitsigns blame_line<CR>", "git blame line")
U.GLMap("gd", "<cmd>DiffviewOpen<CR>", "diffView open")
U.GLMap("gD", "<cmd>DiffviewClose<CR>", "diffView close")
U.GLMap("gh", "<cmd>DiffviewFileHistory %<CR>", "diffView fileHistory")

U.GLMap("tHt", "<cmd>Hardtime toggle<CR>", "toggle Hardtime")
U.GLMap("tHr", "<cmd>Hardtime report<CR>", "Hardtime report")

-- Help menu
U.GLMap("Hm", ":redir @a<CR>:messages<CR>:redir END<CR>:new<CR>:put a<CR>", "messages buffer")
U.GLMap("HL", "<cmd>Lazy<CR>", "open Lazy")
U.GLMap("HM", "<cmd>Mason<CR>", "open Mason")

-- Indent Mode
U.GMap("<", "<gv", "", "v")
U.GMap(">", ">gv", "", "v")

-- LSP menu
-- many lsp functions are actually in nvim-lsp because they need to be
-- in the local LSP callback

-- Marks: Saner mark movement to save keystrokes
U.GMap("'", "`", "")
U.GMap("`", "'", "")

-- Move Lines
U.GMap("<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", "Move Down")
U.GMap("<A-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", "Move Up")
U.GMap("<A-j>", "<esc><cmd>m .+1<cr>==gi", "Move Down", "i")
U.GMap("<A-k>", "<esc><cmd>m .-2<cr>==gi", "Move Up", "i")
U.GMap("<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", "Move Down", "v")
U.GMap("<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", "Move Up", "v")

-- Quickfix Menu
U.GLMap("xn", "<cmd>cnext<CR>zz", "quickfix list next")
U.GLMap("xp", "<cmd>cprev<CR>zz", "quickfix list previous")
U.GLMap("xN", "<cmd>lnext<CR>zz", "loclist next")
U.GLMap("xP", "<cmd>lprev<CR>zz", "loclist previous")
U.GLMap("xo", "<cmd>copen<CR>", "open quickfix list")
U.GLMap("xc", "<cmd>cclose<CR>", "close quickfix list")
U.GLMap("xO", "<cmd>lopen<CR>", "open loclist")
U.GLMap("xC", "<cmd>lclose<CR>", "close loclist")

-- Quit menu
U.GLMap("qq", "<cmd>qa!<CR>", "quit nVim")

-- Quality of Life bindings
-- vertical scroll & center
U.GMap("<C-d>", "<C-d>zz")
U.GMap("<C-u>", "<C-u>zz")

-- treesitter
U.GLMap("uI", "<cmd>InspectTree<cr>", "Inspect Tree")

-- Toggle menu
U.GLMap("to", "<cmd>AerialToggle!<CR>", "outline")
U.GLMap("tw", "<cmd>set wrap!<CR>", "toggle line wrapping")

-- Markdown
U.GLMap("tm", "<cmd>RenderMarkdown toggle<CR>", "toggle RenderMarkdown")

-- Search: behaviour of n and N
U.GMap("n", "'Nn'[v:searchforward].'zzzv'", "Next Search Result", "n", true)
U.GMap("n", "'Nn'[v:searchforward]", "Next Search Result", "x", true)
U.GMap("n", "'Nn'[v:searchforward]", "Next Search Result", "o", true)
U.GMap("N", "'nN'[v:searchforward].'zzzv'", "Prev Search Result", "n", true)
U.GMap("N", "'nN'[v:searchforward]", "Prev Search Result", "x", true)
U.GMap("N", "'nN'[v:searchforward]", "Prev Search Result", "o", true)

-- Search menu
-- Clear highlights on search when pressing <Esc> in normal mode
U.GLMap("sc", "<cmd>nohlsearch<CR>", "clear search highlights")

-- Visual Line movement
U.GMap("j", "gj")
U.GMap("k", "gk")

-- Window movement
-- KMap("c-h", "<cmd>wincmd h<CR>", "move focus to the left window")
-- KMap("c-l", "<cmd>wincmd l<CR>", "move focus to the right window")
-- KMap("c-j", "<cmd>wincmd j<CR>", "move focus to the lower window")
-- KMap("c-k", "<cmd>wincmd k<CR>", "move focus to the upper window")
U.GMap("<C-h>", "<Cmd>NvimTmuxNavigateLeft<CR>")
U.GMap("<C-j>", "<Cmd>NvimTmuxNavigateDown<CR>")
U.GMap("<C-k>", "<Cmd>NvimTmuxNavigateUp<CR>")
U.GMap("<C-l>", "<Cmd>NvimTmuxNavigateRight<CR>")

-- Windows menu
U.GLMap("ws", "<cmd>:split<CR>", "horizontal split")
U.GLMap("wv", "<cmd>:vsplit<CR>", "vertical split")
U.GLMap("wd", "<cmd>:q<CR>", "delete current window")
U.GLMap("wo", "<cmd>:only<CR>", "close all windows except current")

-- Resize windowswith arrows
U.GMap("<C-Up>", ":resize +2<CR>")
U.GMap("<C-Down>", ":resize -2<CR>")
U.GMap("<C-Left>", ":vertical resize +2<CR>")
U.GMap("<C-Right>", ":vertical resize -2<CR>")

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

U.GLMap("wc", ":lua ToggleSplits()<CR>", "change orientation")

-- Yanking
-- don't yank x or visual paste
U.GMap("x", '"_x')
U.GMap("p", '"_dP', "", "v")
