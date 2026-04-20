-- NOTE: Specify the trigger character(s) used for luasnip
local trigger_text = ";"
local cmp_enabled = true

local function toggle_blink()
    cmp_enabled = not cmp_enabled
    vim.notify("blink.cmp " .. (cmp_enabled and "enabled" or "disabled"))
end

-- Custom documentation window (bottom of screen, not anchored to completion menu)
local doc_win_id = nil
local doc_buf_id = nil

local function close_doc_window()
    if doc_win_id and vim.api.nvim_win_is_valid(doc_win_id) then
        vim.api.nvim_win_close(doc_win_id, true)
    end
    doc_win_id = nil
    if doc_buf_id and vim.api.nvim_buf_is_valid(doc_buf_id) then
        vim.api.nvim_buf_delete(doc_buf_id, { force = true })
    end
    doc_buf_id = nil
end

local function get_doc_lines(item)
    local lines = {}

    -- Add detail
    if item.detail and item.detail ~= "" then
        for s in item.detail:gmatch("[^\r\n]+") do
            table.insert(lines, s)
        end
    end

    -- Add documentation
    local doc = item.documentation
    if doc then
        if #lines > 0 then
            table.insert(lines, "")
        end
        if type(doc) == "string" then
            for s in doc:gmatch("[^\r\n]+") do
                table.insert(lines, s)
            end
        elseif doc.value then
            local doc_lines = {}
            vim.lsp.util.convert_input_to_markdown_lines(doc, doc_lines)
            vim.list_extend(lines, doc_lines)
        end
    end

    return lines
end

local function update_doc_content(item)
    if not doc_buf_id or not vim.api.nvim_buf_is_valid(doc_buf_id) then
        return
    end
    local lines = get_doc_lines(item)
    if #lines == 0 then
        lines = { "No documentation available" }
    end
    vim.bo[doc_buf_id].modifiable = true
    vim.api.nvim_buf_set_lines(doc_buf_id, 0, -1, false, lines)
    vim.bo[doc_buf_id].modifiable = false
end

local function show_doc_floating(item)
    -- If the window is already open, just update the content
    if doc_win_id and vim.api.nvim_win_is_valid(doc_win_id) then
        update_doc_content(item)
        return
    end

    local lines = get_doc_lines(item)
    if #lines == 0 then
        vim.notify("No documentation available", vim.log.levels.INFO)
        return
    end

    doc_buf_id = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(doc_buf_id, 0, -1, false, lines)
    vim.bo[doc_buf_id].filetype = "markdown"
    vim.bo[doc_buf_id].modifiable = false

    local editor_width = vim.o.columns
    local editor_height = vim.o.lines
    local win_height = 15
    -- Position: bottom of editor, above statusline (2 = statusline + cmdline)
    local row = editor_height - win_height - 2
    local col = 0
    local win_width = editor_width

    doc_win_id = vim.api.nvim_open_win(doc_buf_id, false, {
        relative = "editor",
        row = row,
        col = col,
        width = win_width,
        height = win_height,
        style = "minimal",
        border = "rounded",
        zindex = 1100,
    })

    vim.wo[doc_win_id].wrap = true
    vim.wo[doc_win_id].linebreak = true
    vim.wo[doc_win_id].conceallevel = 2
    vim.bo[doc_buf_id].textwidth = 100

    -- Update content when selection changes, close when completion hides
    local augroup = vim.api.nvim_create_augroup("BlinkCustomDoc", { clear = true })
    vim.api.nvim_create_autocmd("User", {
        group = augroup,
        pattern = "BlinkCmpListSelect",
        callback = function()
            local new_item = require("blink.cmp").get_selected_item()
            if new_item then
                vim.schedule(function()
                    update_doc_content(new_item)
                end)
            end
        end,
    })
    vim.api.nvim_create_autocmd({ "User", "InsertLeave", "BufLeave" }, {
        group = augroup,
        pattern = { "BlinkCmpHide", "" },
        callback = function(ev)
            -- For User events, only react to BlinkCmpHide
            if ev.event == "User" and ev.match ~= "BlinkCmpHide" then
                return
            end
            vim.schedule(function()
                close_doc_window()
            end)
            vim.api.nvim_del_augroup_by_id(augroup)
        end,
    })
end

local function doc_window_is_open()
    return doc_win_id and vim.api.nvim_win_is_valid(doc_win_id)
end

local function scroll_doc_window(delta)
    if not doc_window_is_open() then
        return false
    end
    vim.schedule(function()
        if not doc_window_is_open() or not doc_buf_id then
            return
        end
        local line_count = vim.api.nvim_buf_line_count(doc_buf_id)
        local win_height = vim.api.nvim_win_get_height(doc_win_id)
        local current = vim.api.nvim_win_get_cursor(doc_win_id)
        local new_line = math.max(1, math.min(current[1] + delta, line_count))
        vim.api.nvim_win_set_cursor(doc_win_id, { new_line, 0 })
        -- Also adjust the viewport scroll position
        local new_top = math.max(1, math.min(new_line, line_count - win_height + 1))
        vim.api.nvim_win_call(doc_win_id, function()
            vim.fn.winrestview({ topline = new_top })
        end)
    end)
    return true
end

local function toggle_custom_doc()
    local item = require("blink.cmp").get_selected_item()
    if not item then
        return require("blink.cmp").show()
    end

    -- Schedule to escape blink's restricted callback context
    vim.schedule(function()
        -- If doc window is open, close it
        if doc_win_id and vim.api.nvim_win_is_valid(doc_win_id) then
            close_doc_window()
            return
        end

        show_doc_floating(item)
        -- To use the split version instead, comment out the line above and uncomment:
        -- show_doc_split(item)
    end)
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
            U.PLMap("Tb", toggle_blink, "blink (current: " .. tostring(cmp_enabled) .. ")"),
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
            -- ["<C-h>"] = { "show", "show_documentation", "hide_documentation" },
            ["<C-h>"] = { toggle_custom_doc },
            ["<C-d>"] = {
                function()
                    return scroll_doc_window(10)
                end,
                "scroll_documentation_down",
                "fallback",
            },
            ["<C-u>"] = {
                function()
                    return scroll_doc_window(-10)
                end,
                "scroll_documentation_up",
                "fallback",
            },
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
                max_height = 6,
                border = "none",
                -- draw = {
                --     components = {
                --         label = {
                --             width = { fill = true, max = 60 },
                --             text = function(ctx)
                --                 local highlights_info = require("colorful-menu").highlights(ctx.item, vim.bo.filetype)
                --                 if highlights_info ~= nil then
                --                     return highlights_info.text
                --                 else
                --                     return ctx.label
                --                 end
                --             end,
                --             highlight = function(ctx)
                --                 local highlights_info = require("colorful-menu").highlights(ctx.item, vim.bo.filetype)
                --                 local highlights = {}
                --                 if highlights_info ~= nil then
                --                     for _, info in ipairs(highlights_info.highlights) do
                --                         table.insert(highlights, {
                --                             info.range[1],
                --                             info.range[2],
                --                             group = ctx.deprecated and "BlinkCmpLabelDeprecated" or info[1],
                --                         })
                --                     end
                --                 end
                --                 for _, idx in ipairs(ctx.label_matched_indices) do
                --                     table.insert(highlights, { idx, idx + 1, group = "BlinkCmpLabelMatch" })
                --                 end
                --                 return highlights
                --             end,
                --         },
                --     },
                -- },
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
                    min_width = 85,
                    max_width = 90,
                    max_height = 30,
                    border = "rounded",
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
