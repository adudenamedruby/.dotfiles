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
        mappings = {
            popup = {
                ["F"] = "PullPopup",
                ["p"] = false,
            },
        },
    },
    KMap("<leader>gs", "<cmd>Neogit<cr>", "NeoGit"),
}
