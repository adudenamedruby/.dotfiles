return {
    "alexghergh/nvim-tmux-navigation",
    config = function()
        require("nvim-tmux-navigation").setup({})
        KMap("<C-h>", "<Cmd>NvimTmuxNavigateLeft<CR>")
        KMap("<C-j>", "<Cmd>NvimTmuxNavigateDown<CR>")
        KMap("<C-k>", "<Cmd>NvimTmuxNavigateUp<CR>")
        KMap("<C-l>", "<Cmd>NvimTmuxNavigateRight<CR>")
    end,
}
