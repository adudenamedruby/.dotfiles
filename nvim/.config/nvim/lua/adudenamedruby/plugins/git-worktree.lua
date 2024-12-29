return {
    "ThePrimeagen/git-worktree.nvim",
    event = "VeryLazy",
    config = function()
        --vim.g.git_worktree_log_level = 'debug'
        require("git-worktree").setup()
        require("telescope").load_extension("git_worktree")
    end,
}
