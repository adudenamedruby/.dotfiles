return {
    "folke/flash.nvim",
    event = "VeryLazy",
    keys = function()
        local flash = require("flash")
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("jj", function()
                flash.jump()
            end, "jump", { "n", "x", "o" }),
            U.PLMap("jt", function()
                flash.treesitter()
            end, "select with treesitter"),
        }
    end,
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
    config = function(_, opts)
        require("flash").setup(opts)

        local ok, wk = pcall(require, "which-key")
        if ok then
            wk.add({
                { "<leader>j", group = "jumps" },
            })
        end
    end,
}
