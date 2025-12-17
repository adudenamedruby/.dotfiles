return {
    "mrjones2014/smart-splits.nvim",
    config = function()
        require("smart-splits").setup({
            -- Ignored filetypes (only while resizing)
            ignored_filetypes = { "nofile", "quickfix", "prompt" },
            -- Ignored buffer types (only while resizing)
            ignored_buftypes = { "NvimTree" },
            -- when moving cursor between splits left or right,
            -- place the cursor on the same row of the *screen*
            -- regardless of line numbers.
            move_cursor_same_row = false,
            -- Whether to wrap to opposite side when cursor is at an edge
            at_edge = "wrap",
        })
    end,
    keys = {
        {
            "<C-h>",
            function()
                require("smart-splits").move_cursor_left()
            end,
            desc = "Move to left split",
        },
        {
            "<C-j>",
            function()
                require("smart-splits").move_cursor_down()
            end,
            desc = "Move to below split",
        },
        {
            "<C-k>",
            function()
                require("smart-splits").move_cursor_up()
            end,
            desc = "Move to above split",
        },
        {
            "<C-l>",
            function()
                require("smart-splits").move_cursor_right()
            end,
            desc = "Move to right split",
        },
        -- Resize splits with Alt+hjkl (optional, but useful)
        {
            "<A-h>",
            function()
                require("smart-splits").resize_left()
            end,
            desc = "Resize split left",
        },
        {
            "<A-j>",
            function()
                require("smart-splits").resize_down()
            end,
            desc = "Resize split down",
        },
        {
            "<A-k>",
            function()
                require("smart-splits").resize_up()
            end,
            desc = "Resize split up",
        },
        {
            "<A-l>",
            function()
                require("smart-splits").resize_right()
            end,
            desc = "Resize split right",
        },
    },
}
