return {
    "mfussenegger/nvim-lint",
    event = { "BufReadPre", "BufNewFile" },
    keys = function()
        local lint = require("lint")
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("ul", function()
                lint.try_lint()
            end, "lint file"),
        }
    end,
    config = function()
        local lint = require("lint")

        lint.linters_by_ft = {
            swift = { "swiftlint" },
        }

        local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

        vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost", "InsertLeave", "TextChanged" }, {
            group = lint_augroup,
            callback = function()
                require("lint").try_lint()
            end,
        })
    end,
}
