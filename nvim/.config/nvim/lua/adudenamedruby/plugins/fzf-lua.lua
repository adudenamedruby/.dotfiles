return {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    -- or if using mini.icons/mini.nvim
    -- dependencies = { "echasnovski/mini.icons" },
    keys = function()
        local fzf = require("fzf-lua")
        local U = require("adudenamedruby.core.utils")
        return {

            U.PLMap("bb", fzf.buffers, "list all buffers"),

            U.PLMap("ff", fzf.files, "fzf find file"),

            U.PLMap("Hk", fzf.keymaps, "search keymaps"),
            U.PLMap("Hb", fzf.builtin, "search fzf-lua builtin"),
            U.PLMap("Hh", fzf.help_tags, "search help"),
            U.PLMap("Hn", function()
                fzf.nvim_options()
            end, "NVim options"),

            U.PLMap("tt", fzf.colorschemes, "themes"),

            U.PLMap("sh", fzf.search_history, "search history"),
            U.PLMap("sH", fzf.command_history, "command history"),
            U.PLMap("sm", fzf.marks, "marks"),
            U.PLMap("sp", fzf.live_grep, "project grep"),
            U.PLMap("sr", fzf.registers, "registers"),
            U.PLMap("sR", fzf.oldfiles, "recent files"),
            U.PLMap("ss", fzf.blines, "swoop buffer"),
            U.PLMap("so", fzf.treesitter, "treesitter"),
            U.PLMap("sd", fzf.diagnostics_workspace, "diagnostics"),
            U.PLMap("sv", function()
                fzf.grep_visual()
            end, "visual selection"),
            U.PLMap("sw", fzf.grep_cword, "current word"),
        }
    end,
    opts = {
        files = {
            -- shorten each path component to 1 char,
            -- but keep the last 2 components intact
            path_shorten = 1,
            path_shorten_exclude = { -1, -2 }, -- keep last 2 full
        },
        keymap = {
            fzf = {
                ["ctrl-q"] = "select-all+accept", -- select everything, then accept
            },
        },
        winopts = {
            -- size/placement (height can be absolute lines)
            height = 20,
            width = 1.0, -- full width
            row = 1, -- near bottom (0 = top, 1 = bottom)
            col = 0, -- left edge

            -- main window border; you can also use: "none", "single", "double", "rounded", "shadow"
            border = "single",

            borderchars = {
                "z", -- top/bottom/left/right as a single char (fallback)
                prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
                results = { " " },
                preview = { " ", "│", " ", "│", " ", " ", " ", " " },
            },

            preview = {
                -- put preview on the right, ~60% of the picker width
                layout = "horizontal",
                horizontal = "right:60%",
                -- optionally hide preview by default:
                -- hidden = "hidden",
            },
        },
    },
}
