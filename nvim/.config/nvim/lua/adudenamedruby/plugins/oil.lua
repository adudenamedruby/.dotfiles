return {
    "stevearc/oil.nvim",
    opts = {},
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        require("oil").setup({
            default_file_explorer = true,
            delete_to_trash = false,
            skip_confirm_for_simple_edits = false,
            view_options = {
                show_hidden = true,
                natural_order = true,
                is_always_hidden = function(name, _)
                    return name == ".." or name == ".git"
                end,
            },
            float = {
                padding = 2,
                max_width = 120,
                max_height = 0,
            },
            win_options = {
                wrap = true,
                winblend = 0,
            },
            keymaps = {
                ["<C-c>"] = false,
                ["q"] = "actions.close",
            },
        })
        KMap("<leader>fo", "<cmd>Oil --float<CR>", "oil")
    end,
}
