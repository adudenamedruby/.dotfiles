return {
    "patrickpichler/hovercraft.nvim",
    dependencies = {
        { "nvim-lua/plenary.nvim" },
    },
    opts = function()
        return {
            providers = {
                providers = {
                    {
                        "LSP",
                        require("hovercraft.provider.lsp.hover").new(),
                    },
                    {
                        "Man",
                        require("hovercraft.provider.man").new(),
                    },
                    {
                        "Dictionary",
                        require("hovercraft.provider.dictionary").new(),
                    },
                },
            },

            window = {
                border = "rounded",
                max_width = 90,
            },

            keys = {
                {
                    "<C-u>",
                    function()
                        require("hovercraft").scroll({ delta = -4 })
                    end,
                },
                {
                    "<C-d>",
                    function()
                        require("hovercraft").scroll({ delta = 4 })
                    end,
                },
                {
                    "<TAB>",
                    function()
                        require("hovercraft").hover_next()
                    end,
                },
                {
                    "<S-TAB>",
                    function()
                        require("hovercraft").hover_next({ step = -1 })
                    end,
                },
            },
        }
    end,
}
