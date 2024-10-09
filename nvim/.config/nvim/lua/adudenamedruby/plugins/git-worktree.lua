return {
    "ThePrimeagen/git-worktree.nvim",
    event = "VeryLazy",
    config = function()
        --vim.g.git_worktree_log_level = 'debug'
        require("git-worktree").setup()
        require("telescope").load_extension("git_worktree")
        KMap("<leader>gs", function()
            require("telescope").extensions.git_worktree.git_worktrees()
        end, "switch worktree")
        KMap("<leader>gc", function()
            require("telescope").extensions.git_worktree.create_git_worktree()
        end, "create worktree")
    end,
}
