return {
    -- quickfix improved workflows
    "stevearc/quicker.nvim",
    event = "FileType qf",

    keys = function()
        local quicker = require("quicker")
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("tq", function()
                quicker.toggle()
            end, "quickfix"),
            U.PLMap("tl", function()
                quicker.toggle({ loclist = true })
            end, "loclist"),
        }
    end,
    config = function()
        require("quicker").setup({
            keys = {
                {
                    ">",
                    function()
                        require("quicker").expand({ before = 2, after = 2, add_to_existing = true })
                    end,
                    desc = "Expand quickfix context",
                },
                {
                    "<",
                    function()
                        require("quicker").collapse()
                    end,
                    desc = "Collapse quickfix context",
                },
            },
        })
    end,
}
