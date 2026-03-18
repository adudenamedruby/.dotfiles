return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            require("nvim-treesitter.config").setup({
                ensure_installed = {
                    "bash",
                    "c",
                    "cpp",
                    "css",
                    "clojure",
                    "diff",
                    "dockerfile",
                    "fennel",
                    "gitignore",
                    "html",
                    "json",
                    "lua",
                    "luadoc",
                    "markdown",
                    "markdown_inline",
                    "python",
                    "regex",
                    "query",
                    "rust",
                    "swift",
                    "toml",
                    "vim",
                    "vimdoc",
                    "yaml",
                },

                auto_install = true,

                -- Incremental selection (plugin module, no native equivalent)
                incremental_selection = {
                    enable = true,
                    keymaps = {
                        node_incremental = "v",
                        node_decremental = "V",
                    },
                },

                -- Textobjects (configured here as required by nvim-treesitter-textobjects)
                textobjects = {
                    select = {
                        enable = true,
                        lookahead = true,
                        keymaps = {
                            ["af"] = "@function.outer",
                            ["if"] = "@function.inner",
                            ["ac"] = "@class.outer",
                            ["ic"] = { query = "@class.inner", desc = "select inner part of a class region" },
                            ["as"] = { query = "@scope", query_group = "locals", desc = "select language scope" },
                        },
                        selection_modes = {
                            ["@parameter.outer"] = "v",
                            ["@function.outer"] = "v",
                            ["@class.outer"] = "<c-v>",
                        },
                        include_surrounding_whitespace = true,
                    },
                },
            })

            -- Native treesitter highlighting (Neovim 0.11+)
            vim.api.nvim_create_autocmd("FileType", {
                callback = function()
                    if pcall(vim.treesitter.start) then
                        -- Ruby needs vim regex highlighting alongside treesitter for indent rules
                        if vim.bo.filetype == "ruby" then
                            vim.bo.syntax = "on"
                        end

                        -- Native treesitter indent for all languages except Ruby
                        if vim.bo.filetype ~= "ruby" then
                            vim.bo.indentexpr = "v:lua.vim.treesitter.indentexpr()"
                        end
                    end
                end,
            })
        end,
    },
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        dependencies = { "nvim-treesitter/nvim-treesitter" },
    },
}
