-- local home = os.getenv("HOME")
-- local swiftformat_config = home .. "/.config/swiftformat/.swiftformat"
--
return {
    -- Autoformat
    "stevearc/conform.nvim",
    event = { "BufWritePre", "BufNewFile" },
    cmd = { "ConformInfo" },
    keys = function()
        local conform = require("conform")
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("uf", function()
                conform.format({
                    async = true,
                    lsp_fallback = true,
                    timeout_ms = 500,
                })
            end, "format buffer"),
        }
    end,
    config = function()
        require("conform").setup({
            notify_on_error = false,
            format_on_save = function(bufnr)
                -- Disable "format_on_save lsp_fallback" for languages that don't
                -- have a well standardized coding style. You can add additional
                -- languages here or re-enable it for the disabled ones.
                local disable_filetypes = {
                    c = true,
                    cpp = true,
                }
                local lsp_format_opt
                if disable_filetypes[vim.bo[bufnr].filetype] then
                    lsp_format_opt = "never"
                else
                    lsp_format_opt = "fallback"
                end
                return {
                    timeout_ms = 500,
                    lsp_format = lsp_format_opt,
                }
            end,
            -- formatters = {
            --     swiftformat = {
            --         command = "swiftformat",
            --         args = {
            --             "--config",
            --             "~/.config/swiftformat/.swiftformat",
            --             "--stdinpath",
            --             "$FILENAME",
            --         },
            --         range_args = function(ctx)
            --             return {
            --                 "--config",
            --                 "~/.config/swiftformat/.swiftformat",
            --                 "--linerange",
            --                 ctx.range.start[1] .. "," .. ctx.range["end"][1],
            --             }
            --         end,
            --         stdin = true,
            --         condition = function(ctx)
            --             return vim.fs.basename(ctx.filename) ~= "README.md"
            --         end,
            --     },
            -- },
            formatters_by_ft = {
                html = { "prettier" },
                json = { "prettier" },
                lua = { "stylua" },
                markdown = { "prettier" },
                rust = { "rustfmt" },
                sh = { "shfmt" },
                -- swift = { "swiftformat" },
                yaml = { "prettier" },
                -- Conform can also run multiple formatters sequentially
                -- python = { "isort", "black" },
                --
                -- You can use 'stop_after_first' to run the first available formatter from the list
                -- javascript = { "prettierd", "prettier", stop_after_first = true },
            },
        })
    end,
}
