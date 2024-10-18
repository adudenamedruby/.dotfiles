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

vim.api.nvim_create_autocmd("BufLeave", {
    pattern = "*NeogitStatus*",
    callback = function()
        vim.cmd("checktime")
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
