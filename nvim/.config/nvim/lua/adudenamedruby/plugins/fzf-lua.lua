return {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    -- or if using mini.icons/mini.nvim
    -- dependencies = { "echasnovski/mini.icons" },
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
