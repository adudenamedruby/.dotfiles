return {
    "stevearc/oil.nvim",
    keys = function()
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("fo", "<cmd>Oil<CR>", "oil"),
        }
    end,
    opts = {},
    -- Optional dependencies
    -- dependencies = { { "nvim-mini/mini.icons", opts = {} } },
    dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if you prefer nvim-web-devicons
    -- Lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
    lazy = false,
}
