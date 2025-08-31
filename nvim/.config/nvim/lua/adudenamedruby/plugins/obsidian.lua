local allowed_statuses = { "editing", "ready" }

return {
    "obsidian-nvim/obsidian.nvim",
    version = "*", -- recommended, use latest release instead of latest commit
    ft = "markdown",

    keys = function()
        -- local obsidian = require("obsidian")
        local U = require("adudenamedruby.core.utils")
        return {

            U.PLMap("on", "<cmd>Obsidian new<CR>", "new note"),
            U.PLMap("ob", "<cmd>Obsidian backlinks<CR>", "show backlinks"),
            U.PLMap("oe", "<cmd>Obsidian extract_note<CR>", "extract selection to note"),
            U.PLMap("ol", "<cmd>Obsidian links<CR>", "pick from note links"),
            U.PLMap("os", "<cmd>Obsidian search<CR>", "search trove"),
            U.PLMap("ot", "<cmd>Obsidian tags<CR>", "tag occurances"),
            U.PLMap("oc", "<cmd>Obsidian toggle_checkbox<CR>", "toggle checkbox"),
            U.PLMap("of", "<cmd>Obsidian follow_link<CR>", "follow link"),
        }
    end,
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

        notes_subdir = "Inbox",
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

        checkbox = {
            order = { " ", "~", "!", ">", "x" },
        },

        legacy_commands = false,
    },

    config = function(_, opts)
        require("obsidian").setup(opts)

        local ok, wk = pcall(require, "which-key")
        if ok then
            wk.add({
                { "<leader>o", group = "Obsidian" },
            })
        end
    end,
}
