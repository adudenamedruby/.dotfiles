local allowed_statuses = { "editing", "ready" }

return {
    "obsidian-nvim/obsidian.nvim",
    version = "*", -- recommended, use latest release instead of latest commit
    ft = "markdown",
    -- event = {
    --     -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
    --     -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/*.md"
    --     -- refer to `:h file-pattern` for more examples
    --     "BufReadPre "
    --         .. vim.fn.expand("~")
    --         .. "Documents/ruby-trove/*.md",
    --     "BufNewFile " .. vim.fn.expand("~") .. "Documents/ruby-trove/*.md",
    -- },
    ---@module 'obsidian'
    ---@type obsidian.config
    opts = {
        -- A list of workspace names, paths, and configuration overrides.
        -- If you use the Obsidian app, the 'path' of a workspace should generally be
        -- your vault root (where the `.obsidian` folder is located).
        -- When obsidian.nvim is loaded by your plugin manager, it will automatically set
        -- the workspace to the first workspace in the list whose `path` is a parent of the
        -- current markdown file being edited.
        workspaces = {
            {
                name = "personal",
                path = vim.fn.expand("~") .. "/Documents/ruby-trove",
            },
        },

        completion = {
            nvim_cmp = false,
            blink = true,
            -- Trigger completion at 2 chars.
            min_chars = 2,
            -- Set to false to disable new note creation in the picker
            create_new = true,
        },

        notes_subdir = "Index",
        -- Where to put new notes. Valid options are
        -- _ "current_dir" - put new notes in same directory as the current buffer.
        -- _ "notes_subdir" - put new notes in the default notes subdirectory.
        new_notes_location = "notes_subdir",

        -- Optional, boolean or a function that takes a filename and returns a boolean.
        -- `true` indicates that you don't want obsidian.nvim to manage frontmatter.
        disable_frontmatter = false,

        -- Optional, alternatively you can customize the frontmatter data.
        ---@return table
        note_frontmatter_func = function(note)
            local date_created = os.date("%Y/%m/%d - %H:%M:%S")

            local uuid = ""
            for _ = 1, 4 do
                uuid = uuid .. string.char(math.random(65, 90))
            end
            uuid = tostring(os.date("%Y%m%d%H%M%S") .. "-" .. uuid)

            local out = {
                id = uuid,
                created = date_created,
                tags = note.tags,
                hubs = {},
                filing_status = allowed_statuses[1],
            }

            -- `note.metadata` contains any manually added fields in the frontmatter.
            -- So here we just make sure those fields are kept in the frontmatter.
            if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
                for k, v in pairs(note.metadata) do
                    out[k] = v
                end
            end

            return out
        end,

        -- Optional, customize how note IDs are generated given an optional title.
        ---@param title string|?
        ---@return string
        note_id_func = function(title)
            -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
            -- In this case a note with the title 'My new note' will be given an ID that looks
            -- like '1657296016-my-new-note', and therefore the file name '1657296016-my-new-note.md'.
            -- You may have as many periods in the note ID as you'd like—the ".md" will be added automatically
            -- local suffix = ""
            if title ~= nil then
                -- If title is given, transform it into valid file name.
                -- suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
                return title
            else
                -- If title is nil, just add 4 random uppercase letters to the suffix.
                -- for _ = 1, 4 do
                --     suffix = suffix .. string.char(math.random(65, 90))
                -- end
                return tostring(os.date("%Y%m%d%H%M%S") .. " - New Note")
            end
            -- return tostring(os.time()) .. "-" .. suffix
        end,

        templates = {
            folder = "Templates",
            date_format = "%Y-%m-%d",
            time_format = "%H:%M",
        },

        picker = {
            -- Set your preferred picker. Can be one of 'telescope.nvim', 'fzf-lua', 'mini.pick' or 'snacks.pick'.
            name = "fzf-lua",
            -- Optional, configure key mappings for the picker. These are the defaults.
            -- Not all pickers support all mappings.
            note_mappings = {
                -- Create a new note from your query.
                new = "<C-x>",
                -- Insert a link to the selected note.
                insert_link = "<C-l>",
            },
            tag_mappings = {
                -- Add tag(s) to current note.
                tag_note = "<C-x>",
                -- Insert a tag at the current location.
                insert_tag = "<C-l>",
            },
        },

        -- Optional, determines how certain commands open notes. The valid options are:
        -- 1. "current" (the default) - to always open in the current window
        -- 2. "vsplit" - only open in a vertical split if a vsplit does not exist.
        -- 3. "hsplit" - only open in a horizontal split if a hsplit does not exist.
        -- 4. "vsplit_force" - always open a new vertical split if the file is not in the adjacent vsplit.
        -- 5. "hsplit_force" - always open a new horizontal split if the file is not in the adjacent hsplit.
        open_notes_in = "current",

        -- Optional, configure additional syntax highlighting / extmarks.
        -- This requires you have `conceallevel` set to 1 or 2. See `:help conceallevel` for more details.
        ui = {
            enable = true, -- set to false to disable all additional syntax features
            ignore_conceal_warn = false, -- set to true to disable conceallevel specific warning
            update_debounce = 200, -- update delay after a text change (in milliseconds)
            max_file_length = 5000, -- disable UI features for files with more than this many lines
            -- Define how various check-boxes are displayed
            checkboxes = {
                -- NOTE: the 'char' value has to be a single character, and the highlight groups are defined below.
                [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
                ["x"] = { char = "", hl_group = "ObsidianDone" },
                [">"] = { char = "", hl_group = "ObsidianRightArrow" },
                ["~"] = { char = "󰰱", hl_group = "ObsidianTilde" },
                ["!"] = { char = "", hl_group = "ObsidianImportant" },
                -- Replace the above with this if you don't have a patched font:
                -- [" "] = { char = "☐", hl_group = "ObsidianTodo" },
                -- ["x"] = { char = "✔", hl_group = "ObsidianDone" },

                -- You can also add more custom ones...
            },
            -- Use bullet marks for non-checkbox lists.
            bullets = { char = "•", hl_group = "ObsidianBullet" },
            external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
            -- Replace the above with this if you don't have a patched font:
            -- external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
            reference_text = { hl_group = "ObsidianRefText" },
            highlight_text = { hl_group = "ObsidianHighlightText" },
            tags = { hl_group = "ObsidianTag" },
            block_ids = { hl_group = "ObsidianBlockID" },
            hl_groups = {
                -- The options are passed directly to `vim.api.nvim_set_hl()`. See `:help nvim_set_hl`.
                ObsidianTodo = { bold = true, fg = "#f78c6c" },
                ObsidianDone = { bold = true, fg = "#89ddff" },
                ObsidianRightArrow = { bold = true, fg = "#f78c6c" },
                ObsidianTilde = { bold = true, fg = "#ff5370" },
                ObsidianImportant = { bold = true, fg = "#d73128" },
                ObsidianBullet = { bold = true, fg = "#89ddff" },
                ObsidianRefText = { underline = true, fg = "#c792ea" },
                ObsidianExtLinkIcon = { fg = "#c792ea" },
                ObsidianTag = { italic = true, fg = "#89ddff" },
                ObsidianBlockID = { italic = true, fg = "#89ddff" },
                ObsidianHighlightText = { bg = "#75662e" },
            },
        },

        checkbox = {
            order = { " ", "~", "!", ">", "x" },
        },
    },
}
