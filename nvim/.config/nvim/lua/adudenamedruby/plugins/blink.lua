return {
    -- TODO: if turning this on, you need to enable capabilites in lsp
    --
    "saghen/blink.cmp",
    lazy = false, -- lazy loading handled internally
    dependencies = "rafamadriz/friendly-snippets",

    -- use a release tag to download pre-built binaries
    version = "v0.*",

    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
        keymap = {
            preset = "default",
            ["<C-h>"] = { "show", "show_documentation", "hide_documentation" },
            ["<C-t>"] = { "select_and_accept" },
        },

        appearance = {
            use_nvim_cmp_as_default = true,
            nerd_font_variant = "mono",
        },

        sources = {
            default = { "lsp", "path", "snippets", "buffer" },
            -- optionally disable cmdline completions
            -- cmdline = {},
        },
        completion = {
            list = {
                max_items = 50,
            },

            menu = {
                min_width = 15,
                max_height = 10,
                border = "none",
            },

            documentation = {
                -- Controls whether the documentation window will automatically show when selecting a completion item
                auto_show = false,
                -- Delay before showing the documentation window
                auto_show_delay_ms = 500,
                -- Delay before updating the documentation window when selecting a new item,
                -- while an existing item is still visible
                update_delay_ms = 50,
                -- Whether to use treesitter highlighting, disable if you run into performance issues
                treesitter_highlighting = true,
                window = {
                    min_width = 30,
                    max_width = 90,
                    max_height = 30,
                    border = "padded",
                },
            },
        },

        signature = { enabled = true },
    },
    opts_extend = { "sources.default" },
}
