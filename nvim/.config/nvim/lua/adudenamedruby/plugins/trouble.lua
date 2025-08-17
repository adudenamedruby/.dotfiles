return {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = { "BufReadPre", "BufNewFile" },
    keys = function()
        local U = require("adudenamedruby.core.utils")

        return {
            U.PLMap("dt", "<cmd>Trouble diagnostics toggle<CR>", "Trouble (project)"),
            U.PLMap("dt", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", "Trouble (buffer)"),
            U.PLMap("xT", "<cmd>Trouble loclist toggle<cr>", "toggle loclist (Trouble)"),
            U.PLMap("xt", "<cmd>Trouble gflist toggle<cr>", "toggle quickfix (Trouble)"),
        }
    end,
    config = function()
        require("trouble").setup({
            auto_open = false,
            auto_close = false,
            auto_preview = true,
            auto_jump = false,
            mode = "quickfix",
            severity = vim.diagnostic.severity.WARN,
            cycle_results = false,
        })

        vim.api.nvim_create_autocmd("User", {
            pattern = { "XcodebuildBuildFinished", "XcodebuildTestsFinished" },
            callback = function(event)
                if event.data.cancelled then
                    return
                end

                if event.data.success then
                    require("trouble").close()
                elseif not event.data.failedCount or event.data.failedCount > 0 then
                    if next(vim.fn.getqflist()) then
                        require("trouble").open("quickfix")
                    else
                        require("trouble").close()
                    end

                    require("trouble").refresh()
                end
            end,
        })
    end,
}
