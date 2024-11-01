-- local home = os.getenv("HOME")
-- local swiftformat_config = home .. "/.config/swiftformat/.swiftformat"
--
return {
    -- Autoformat
    "stevearc/conform.nvim",
    event = { "BufWritePre", "BufNewFile" },
    cmd = { "ConformInfo" },
    keys = {
        {
            "<leader>uf",
            function()
                require("conform").format({
                    async = true,
                    lsp_fallback = true,
                    timeout_ms = 500,
                })
            end,
            mode = "n",
            desc = "format buffer",
        },
    },
    opts = {
        notify_on_error = false,
        format_on_save = function(bufnr)
            -- Disable "format_on_save lsp_fallback" for languages that don't
            -- have a well standardized coding style. You can add additional
            -- languages here or re-enable it for the disabled ones.
            local disable_filetypes = { c = true, cpp = true }
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
        formatters = {
            swiftformat = {
                command = "swiftformat",
                args = {
                    "--config",
                    "~/.config/nvim/.swiftformat", -- update fallback path if needed
                    "--stdinpath",
                    "$FILENAME",
                },
                range_args = function(ctx)
                    return {
                        "--config",
                        "~/.config/nvim/.swiftformat", -- update fallback path if needed
                        "--linerange",
                        ctx.range.start[1] .. "," .. ctx.range["end"][1],
                    }
                end,
                stdin = true,
                condition = function(ctx)
                    return vim.fs.basename(ctx.filename) ~= "README.md"
                end,
            },
        },
        formatters_by_ft = {
            sh = { "shfmt" },
            html = { "prettier" },
            json = { "prettier" },
            yaml = { "prettier" },
            markdown = { "prettier" },
            lua = { "stylua" },
            swift = { "swiftformat" },
            -- Conform can also run multiple formatters sequentially
            -- python = { "isort", "black" },
            --
            -- You can use 'stop_after_first' to run the first available formatter from the list
            -- javascript = { "prettierd", "prettier", stop_after_first = true },
        },
    },
}
