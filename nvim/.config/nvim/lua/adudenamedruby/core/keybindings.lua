-- Keymappings

-- Aerial
KMap("<leader>ta", "<cmd>AerialToggle!<CR>", "aerial")

-- Code menu
WKMapGroup("c", "code")

-- Conform
WKMap("uf", function()
    require("conform").format({
        async = true,
        lsp_fallback = true,
        timeout_ms = 500,
    })
end, "format buffer")

-- Buffer menu
WKMapGroup("b", "buffers")
KMap("<leader><TAB>", "<cmd>:b#<CR>", "switch to last buffer")
KMap("<leader>be", "<cmd>:enew<CR>", "open empty buffer")
KMap("<leader>bd", "<cmd>:bd<CR>", "delete buffer")
KMap("<leader>bD", "<cmd>:bd!<CR>", "force delete buffer")
KMap("<leader>bo", "<cmd>:bd!# | e#<CR>", "force delete buffer")
KMap("<leader>bn", "<cmd>:bn<CR>", "next buffer")
KMap("<leader>bp", "<cmd>:bp<CR>", "previous buffer")
KMap("<leader>br", ":e<CR>:bd#<CR>:e<CR>", "reload buffer with file")

-- Debug menu
WKMapGroup("d", "debug")
KMap("<leader>dp", vim.diagnostic.goto_prev, "go to previous diagnostic")
KMap("<leader>dn", vim.diagnostic.goto_next, "go to next diagnostic")
KMap("<leader>dl", vim.diagnostic.open_float, "show line diagnostics")
KMap("<leader>dq", vim.diagnostic.setloclist, "open diagnostic quickfix list")

-- Error menu
WKMapGroup("e", "errors")
KMap("<leader>en", "<cmd>silent cc | silent cn<cr>zz", "jump to next issue")
KMap("<leader>ep", "<cmd>silent cc | silent cp<cr>zz", "jump to previous issue")

-- Files menu
WKMapGroup("f", "files")
KMap("<leader>fs", "<cmd>w<CR>", "save file")

-- flash
local flash = require("flash")

KMap("<leader>j", function()
    flash.jump()
end, "jump", { "n", "x", "o" })
KMap("<leader>us", function()
    flash.treesitter()
end, "select with treesitter")

-- Git menu
WKMapGroup("g", "git")
KMap("<leader>gb", "<cmd>Gitsigns blame<CR>", "git blame")
KMap("<leader>gl", "<cmd>Gitsigns blame_line<CR>", "git blame line")

-- Harpoon menu
WKMapGroup("h", "harpoon")

-- Help menu
WKMapGroup("H", "Help")
KMap("<leader>Hm", ":redir @a<CR>:messages<CR>:redir END<CR>:new<CR>:put a<CR>", "messages buffer")
KMap("<leader>HM", "<cmd>Mason<CR>", "open Mason")

-- Indent Mode
KMap("<", "<gv", "", "v")
KMap(">", ">gv", "", "v")

-- LSP menu
WKMapGroup("l", "LSP")

-- Marks: Saner mark movement to save keystrokes
KMap("'", "`", "")
KMap("`", "'", "")

-- Move Lines
KMap("<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", "Move Down")
KMap("<A-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", "Move Up")
KMap("<A-j>", "<esc><cmd>m .+1<cr>==gi", "Move Down", "i")
KMap("<A-k>", "<esc><cmd>m .-2<cr>==gi", "Move Up", "i")
KMap("<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", "Move Down", "v")
KMap("<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", "Move Up", "v")

-- Quickfix Menu
WKMapGroup("x", "quickfix")
KMap("<leader>xn", "<cmd>cnext<CR>zz", "quickfix list next")
KMap("<leader>xp", "<cmd>cprev<CR>zz", "quickfix list previous")
KMap("<leader>xN", "<cmd>lnext<CR>zz", "loclist next")
KMap("<leader>xP", "<cmd>lprev<CR>zz", "loclist previous")
KMap("<leader>xo", "<cmd>copen<CR>", "open quickfix list")
KMap("<leader>xc", "<cmd>cclose<CR>", "close quickfix list")
KMap("<leader>xO", "<cmd>lopen<CR>", "open loclist")
KMap("<leader>xC", "<cmd>lclose<CR>", "close loclist")

-- Quit menu
WKMapGroup("q", "quit")
KMap("<leader>qq", "<cmd>qa!<CR>", "quit nVim")

-- Quality of Life bindings
-- vertical scroll & center
KMap("<C-d>", "<C-d>zz")
KMap("<C-u>", "<C-u>zz")

-- treesitter
KMap("<leader>uI", "<cmd>InspectTree<cr>", "Inspect Tree")

-- Toggle menu
WKMapGroup("t", "toggle")
KMap("<leader>tw", "<cmd>set wrap!<CR>", "toggle line wrapping")

-- Search: behaviour of n and N
KMap("n", "'Nn'[v:searchforward].'zzzv'", "Next Search Result", "n", true)
KMap("n", "'Nn'[v:searchforward]", "Next Search Result", "x", true)
KMap("n", "'Nn'[v:searchforward]", "Next Search Result", "o", true)
KMap("N", "'nN'[v:searchforward].'zzzv'", "Prev Search Result", "n", true)
KMap("N", "'nN'[v:searchforward]", "Prev Search Result", "x", true)
KMap("N", "'nN'[v:searchforward]", "Prev Search Result", "o", true)

-- Search menu
WKMapGroup("s", "search")
-- Clear highlights on search when pressing <Esc> in normal mode
KMap("<leader>sc", "<cmd>nohlsearch<CR>", "clear search highlights")

-- Utilites menu
WKMapGroup("u", "utilities")

-- Visual Line movement
KMap("j", "gj")
KMap("k", "gk")

-- Window movement
KMap("c-h", "<cmd>wincmd h<CR>", "move focus to the left window")
KMap("c-l", "<cmd>wincmd l<CR>", "move focus to the right window")
KMap("c-j", "<cmd>wincmd j<CR>", "move focus to the lower window")
KMap("c-k", "<cmd>wincmd k<CR>", "move focus to the upper window")

-- Windows menu
WKMapGroup("w", "windows")
WKMap("ws", "<cmd>:split<CR>", "horizontal split")
WKMap("wv", "<cmd>:vsplit<CR>", "vertical split")
WKMap("wd", "<cmd>:q<CR>", "delete current window")
WKMap("wo", "<cmd>:only<CR>", "close all windows except current")

-- Resize windowswith arrows
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

KMap("<leader>wc", ":lua ToggleSplits()<CR>", "change orientation")

-- Yanking
-- don't yank x or visual paste
KMap("x", '"_x')
KMap("p", '"_dP', "", "v")
