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
    KMap("<leader>j", function()
        require("flash").jump()
    end, "jump", { "n", "x", "o" }),
    KMap("<leader>us", function()
        require("flash").treesitter()
    end, "select with treesitter"),
    -- KMap("R", function()
    --     require("flash").remote()
    -- end, "Remote Flash", "o"),
}
