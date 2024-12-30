-- Global Settings

-- Set <space> as the leader key
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Disable netrw because we'll be using other file browsers
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.g.have_nerd_font = true

-- QoL Autocommands

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd("TextYankPost", {
    desc = "Highlight when yanking (copying) text",
    group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

-- In the quickfix window, <CR> is used to jump to the error under the
-- cursor, so undefine the mapping there.
vim.api.nvim_create_autocmd("FileType", {
    pattern = "qf",
    callback = function()
        vim.api.nvim_buf_set_keymap(0, "n", "<CR>", "<CR>", { noremap = true, silent = true })
    end,
})

-- Enable conceal for specific filetypes
-- vim.api.nvim_create_autocmd("FileType", {
--     pattern = {
--         "swift",
--         "rust",
--         "clojure",
--         "lisp",
--     },
--     callback = function()
--         vim.cmd([[
--             syntax match Lambda "func" conceal cchar=λ
--         ]])
--     end,
-- })
