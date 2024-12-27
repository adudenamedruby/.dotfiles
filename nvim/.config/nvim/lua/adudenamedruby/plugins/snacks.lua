return {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    ---@type snacks.Config
    opts = {
        indent = { enabled = true },
        quickfile = { enabled = true },
        scope = { enabled = true },
        statuscolumn = { enabled = true },
    },
}
