return {
    "folke/flash.nvim",
    event = "VeryLazy",
    ---@type Flash.Config
    opts = {
        jump = {
            autojump = true,
        },
        modes = {
            char = {
                -- avy jump jump
                enabled = false,
                jump_labels = true,
                multi_line = false,
            },
        },
    },
}
