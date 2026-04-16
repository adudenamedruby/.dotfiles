return {
    "dlyongemallo/diffview.nvim",
    version = "*",
    lazy = false,
    keys = function()
        local U = require("adudenamedruby.core.utils")
        return {
            U.GLMap("gd", "<cmd>DiffviewOpen<CR>", "diffView open"),
            U.GLMap("gD", "<cmd>DiffviewClose<CR>", "diffView close"),
            U.GLMap("gh", "<cmd>DiffviewFileHistory %<CR>", "diffView fileHistory"),
        }
    end,
    config = function()
        require("diffview").setup({
            enhanced_diff_hl = true,
            use_icons = true,
            view = {
                default = { layout = "diff2_horizontal" },
                merge_tool = { layout = "diff3_horizontal" },
            },
            file_panel = {
                listing_style = "tree",
                win_config = { position = "left", width = 35 }, -- Use "auto" to fit content
            },
        })
    end,
}
