-- Options

-- Sync clipboard between OS and Neovim.
--  Schedule the setting after `UiEnter` because it can increase startup-time. Schedule the setting after `UiEnter` because it can increase startup-time. Schedule the setting after `UiEnter` because it can increase startup-time. Schedule the setting after `UiEnter` because it can increase startup-time. Schedule the setting after `UiEnter` because it can increase startup-time.
--  Schedule the setting after `UiEnter` because it can increase startup-time.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.schedule(function()
    vim.opt.clipboard = "unnamedplus"
end)
-- vim.g.clipboard = {
--     name = "OSC 52",
--     copy = {
--         ["+"] = require("vim.ui.clipboard.osc52").copy("+"),
--         ["*"] = require("vim.ui.clipboard.osc52").copy("*"),
--     },
--     paste = {
--         ["+"] = require("vim.ui.clipboard.osc52").paste("+"),
--         ["*"] = require("vim.ui.clipboard.osc52").paste("*"),
--     },
-- }

-- Customize the tabline display to show the full file path
function _G.CustomTabLine()
    -- Get the current buffer name
    local bufname = vim.fn.bufname(vim.fn.bufnr())
    if bufname == "" then
        return "[No Name]"
    end

    -- Get the relative path of the file with respect to the project root
    local filepath = vim.fn.fnamemodify(bufname, ":.")

    -- Return the relative path, or full path if it cannot be made relative
    return filepath ~= "" and filepath or vim.fn.fnamemodify(bufname, ":p")
end

local undodir = vim.fn.expand("~/.vim/undodir")
if vim.fn.isdirectory(undodir) == 0 then
    vim.fn.mkdir(undodir, "p")
end

local options = {
    -- general line stuff
    cursorline = true, -- highlight the current line
    number = true, -- set numbered lines
    numberwidth = 4, -- set number column width to 2 {default 4}
    scrolloff = 100, -- is one of my fav
    sidescrolloff = 8,
    wrap = false, -- display lines as one long line

    -- tabs
    expandtab = true, -- convert tabs to spaces
    shiftwidth = 4,
    smartindent = true, -- make indenting smarter again
    autoindent = true, -- copy indent from current line
    softtabstop = 4,
    tabstop = 4,

    -- search
    hlsearch = true, -- highlight all matches on previous search pattern
    ignorecase = true, -- ignore case in search patterns
    incsearch = true,
    smartcase = true, -- smart case

    -- other
    autochdir = false,
    breakindent = true,
    clipboard = "unnamedplus", -- allows neovim to access the system clipboard
    cmdheight = 2, -- more space in the neovim command line for displaying messages
    colorcolumn = "90",
    completeopt = { "menuone", "noselect" }, -- mostly just for cmp
    conceallevel = 2, -- so that `` is visible in markdown files
    fileencoding = "utf-8", -- the encoding written to a file
    foldenable = false,
    foldmethod = "manual",
    inccommand = "split",
    --isfname:append("@-@"),
    linebreak = true, -- don't split words
    listchars = {
        tab = "» ",
        trail = "·",
        nbsp = "¬",
        space = "·",
        precedes = "«",
        extends = "»",
    },
    list = true,
    mouse = "a", -- allow the mouse to be used in neovim
    nu = true,
    pumheight = 10, -- pop up menu height
    relativenumber = true, -- set relative numbered lines
    showmode = false, -- we don't need to see things like -- INSERT -- anymore
    showtabline = 2, -- always show tabs
    signcolumn = "yes", -- always show the sign column, otherwise it would shift the text each time
    splitbelow = true, -- force all horizontal splits to go below current window
    splitright = true, -- force all vertical splits to go to the right of current window
    termguicolors = true,
    tabline = "%!v:lua.CustomTabLine()",
    timeoutlen = 500, -- time to wait for a mapped sequence to complete (in milliseconds)
    updatetime = 250,
    --
    -- File history
    autoread = true, -- auto-reload changes outside of neovim
    autowrite = false, -- do not auto-save
    backup = false, -- creates a backup file
    swapfile = false, -- creates a swapfile
    undofile = true, -- enable persistent undo
    undodir = undodir,
    writebackup = false, -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
}

for key, value in pairs(options) do
    vim.opt[key] = value
end
