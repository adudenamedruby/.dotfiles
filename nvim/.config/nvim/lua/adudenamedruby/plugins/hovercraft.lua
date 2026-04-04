return {
    "patrickpichler/hovercraft.nvim",
    dependencies = {
        { "nvim-lua/plenary.nvim" },
    },
    keys = function()
        local hovercraft = require("hovercraft")
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("ch", function()
                if hovercraft.is_visible() then
                    hovercraft.enter_popup()
                else
                    hovercraft.hover()
                end
            end, "lsp info help"),
        }
    end,
    config = function(_, opts)
        local hovercraft = require("hovercraft")
        hovercraft.setup(opts)

        -- Reposition the hover window after it appears
        hovercraft.ui:register_onshow(function(_bufnr)
            vim.schedule(function()
                local wc = hovercraft.ui.window_config
                if not wc or not vim.api.nvim_win_is_valid(wc.winnr) then
                    return
                end

                local width = vim.api.nvim_win_get_width(0)
                local height = 20
                vim.api.nvim_win_set_config(wc.winnr, {
                    relative = "editor",
                    row = vim.o.lines - height - 2,
                    col = 0,
                    width = width,
                    height = height,
                })
            end)
        end)
    end,
    opts = function()
        return {
            providers = {
                providers = {
                    {
                        "LSP",
                        require("hovercraft.provider.lsp.hover").new(),
                    },
                    {
                        "Dictionary",
                        require("hovercraft.provider.dictionary").new(),
                    },
                    {
                        "Man",
                        require("hovercraft.provider.man").new(),
                    },
                },
            },

            window = {
                border = "rounded",
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
