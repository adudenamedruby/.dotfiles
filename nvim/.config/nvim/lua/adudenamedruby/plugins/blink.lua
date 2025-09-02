-- NOTE: Specify the trigger character(s) used for luasnip
local trigger_text = ";"
local cmp_enabled = true

local function toggle_blink()
    cmp_enabled = not cmp_enabled
    vim.notify("blink.cmp " .. (cmp_enabled and "enabled" or "disabled"))
end

return {
    "saghen/blink.cmp",
    lazy = false, -- lazy loading handled internally
    dependencies = {
        "rafamadriz/friendly-snippets",
        {
            "saghen/blink.compat",
            optional = true, -- make optional so it's only enabled if any extras need it
            opts = {},
            version = not vim.g.lazyvim_blink_main and "*",
        },
    },

    -- use a release tag to download pre-built binaries
    version = "*",

    keys = function()
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("tb", toggle_blink, "blink (current: " .. tostring(cmp_enabled) .. ")"),
        }
    end,

    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
        enabled = function()
            -- Get the current buffer's filetype
            local filetype = vim.bo[0].filetype
            -- Disable for Telescope buffers
            if filetype == "TelescopePrompt" or filetype == "minifiles" then
                return false
            end
            return cmp_enabled
        end,
        keymap = {
            preset = "default",
            ["<C-h>"] = { "show", "show_documentation", "hide_documentation" },
            ["<C-s>"] = { "select_and_accept" },
        },

        appearance = {
            use_nvim_cmp_as_default = true,
            nerd_font_variant = "mono",
        },
        sources = {
            default = { "lsp", "path", "snippets", "buffer" },
            providers = {
                lsp = {
                    name = "lsp",
                    enabled = true,
                    module = "blink.cmp.sources.lsp",
                    score_offset = 90,
                },
                snippets = {
                    name = "snippets",
                    enabled = true,
                    max_items = 8,
                    min_keyword_length = 2,
                    module = "blink.cmp.sources.snippets",
                    score_offset = 70,
                    -- Only show snippets if I type the trigger_text characters, so
                    -- to expand the "bash" snippet, if the trigger_text is ";" I have to
                    should_show_items = true,
                    -- function()
                    --     local col = vim.api.nvim_win_get_cursor(0)[2]
                    --     local before_cursor = vim.api.nvim_get_current_line():sub(1, col)
                    --     -- NOTE: remember that `trigger_text` is modified at the top of the file
                    --     return before_cursor:match(trigger_text .. "%w*$") ~= nil
                    -- end,
                    -- After accepting the completion, delete the trigger_text characters
                    -- from the final inserted text
                    -- transform_items = function(_, items)
                    --     local col = vim.api.nvim_win_get_cursor(0)[2]
                    --     local before_cursor = vim.api.nvim_get_current_line():sub(1, col)
                    --     local trigger_pos = before_cursor:find(trigger_text .. "[^" .. trigger_text .. "]*$")
                    --     if trigger_pos then
                    --         for _, item in ipairs(items) do
                    --             item.textEdit = {
                    --                 newText = item.insertText or item.label,
                    --                 range = {
                    --                     start = { line = vim.fn.line(".") - 1, character = trigger_pos - 1 },
                    --                     ["end"] = { line = vim.fn.line(".") - 1, character = col },
                    --                 },
                    --             }
                    --         end
                    --     end
                    --     -- NOTE: After the transformation, I have to reload the luasnip source
                    --     -- Otherwise really crazy shit happens and I spent way too much time
                    --     -- figurig this out
                    --     vim.schedule(function()
                    --         require("blink.cmp").reload("snippets")
                    --     end)
                    --     return items
                    -- end,
                },
                path = {
                    name = "Path",
                    module = "blink.cmp.sources.path",
                    score_offset = 25,
                    fallbacks = { "snippets", "buffer" },
                    opts = {
                        trailing_slash = false,
                        label_trailing_slash = true,
                        get_cwd = function(context)
                            return vim.fn.expand(("#%d:p:h"):format(context.bufnr))
                        end,
                        show_hidden_files_by_default = true,
                    },
                },
                buffer = {
                    name = "Buffer",
                    enabled = true,
                    max_items = 3,
                    module = "blink.cmp.sources.buffer",
                    min_keyword_length = 3,
                    score_offset = 15,
                },
            },
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
                    min_width = 80,
                    max_width = 90,
                    max_height = 30,
                    border = "padded",
                    direction_priority = {
                        menu_north = { "n", "w", "s", "e" },
                        menu_south = { "s", "e", "n", "w" },
                    },
                },
            },
        },
        signature = { enabled = true },
        -- snippets = {
        --     preset = "luasnip",
        --     -- This comes from the luasnip extra, if you don't add it, won't be able to
        --     -- jump forward or backward in luasnip snippets
        --     -- https://www.lazyvim.org/extras/coding/luasnip#blinkcmp-optional
        --     expand = function(snippet)
        --         require("luasnip").lsp_expand(snippet)
        --     end,
        --     active = function(filter)
        --         if filter and filter.direction then
        --             return require("luasnip").jumpable(filter.direction)
        --         end
        --         return require("luasnip").in_snippet()
        --     end,
        --     jump = function(direction)
        --         require("luasnip").jump(direction)
        --     end,
        -- },
    },
    opts_extend = { "sources.default" },
}
