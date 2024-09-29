return {
    "NeogitOrg/neogit",
    dependencies = {
        "nvim-lua/plenary.nvim",
        "sindrets/diffview.nvim",
        "nvim-telescope/telescope.nvim",
    },
    config = true,
    opts = {
        disable_hint = true,
        disable_insert_on_commit = true,
        intogrations = {
            diffview = true,
        },
        mappings = {
            popup = {
                ["F"] = "PullPopup",
                ["p"] = false,
            },
        },
    },
    KMap("<leader>gs", "<cmd>Neogit<cr>", "NeoGit"),
}
