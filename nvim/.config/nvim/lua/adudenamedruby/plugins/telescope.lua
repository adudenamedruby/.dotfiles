return {
    "nvim-telescope/telescope.nvim",
    event = "VimEnter",
    branch = "0.1.x",
    dependencies = {
        "nvim-lua/plenary.nvim",
        { -- If encountering errors, see telescope-fzf-native README for installation instructions
            "nvim-telescope/telescope-fzf-native.nvim",

            -- `build` is used to run some command when the plugin is installed/updated.
            -- This is only run then, not every time Neovim starts up.
            build = "make",

            -- `cond` is a condition used to determine whether this plugin should be
            -- installed and loaded.
            cond = function()
                return vim.fn.executable("make") == 1
            end,
        },
        { "nvim-telescope/telescope-ui-select.nvim" },

        -- Useful for getting pretty icons, but requires a Nerd Font.
        { "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
    },
    config = function()
        -- Telescope is a fuzzy finder that comes with a lot of different things that
        -- it can fuzzy find! It's more than just a "file finder", it can search
        -- many different aspects of Neovim, your workspace, LSP, and more!
        --
        -- The easiest way to use Telescope, is to start by doing something like:
        --  :Telescope help_tags
        --
        -- After running this command, a window will open up and you're able to
        -- type in the prompt window. You'll see a list of `help_tags` options and
        -- a corresponding preview of the help.
        --
        -- Two important keymaps to use while in Telescope are:
        --  - Insert mode: <c-/>
        --  - Normal mode: ?
        --
        -- This opens a window that shows you all of the keymaps for the current
        -- Telescope picker. This is really useful to discover what Telescope can
        -- do as well as how to actually do it!

        local pickerLayout = {
            theme = "ivy",
            layout_config = {
                height = 20,
                preview_width = 0.6,
            },
            borderchars = {
                "z",
                prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
                results = { " " },
                -- results = { "a", "b", "c", "d", "e", "f", "g", "h" },
                -- preview = { " " },
                preview = { " ", "│", " ", "│", " ", " ", " ", " " },
                -- preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
            },
        }

        -- [[ Configure Telescope ]]
        -- See `:help telescope` and `:help telescope.setup()`
        require("telescope").setup({
            -- You can put your default mappings / updates / etc. in here
            --  All the info you're looking for is in `:help telescope.setup()`
            --
            defaults = {
                -- layout_strategy = "ivy",
                -- layout_config = {
                --   height = 0.5,
                --   -- other layout_config options
                -- },
                path_display = {
                    shorten = {
                        len = 1,
                        exclude = { -1, -2 },
                    },
                },
                -- mappings = {
                --     i = { ["<c-enter>"] = "to_fuzzy_refine" },
                -- },
            },
            pickers = {
                find_files = pickerLayout,
                git_files = pickerLayout,
                grep_string = pickerLayout,
                live_grep = pickerLayout,
                buffers = pickerLayout,
                old_files = pickerLayout,
                commands = pickerLayout,
                tags = pickerLayout,
                command_history = pickerLayout,
                search_history = pickerLayout,
                help_tags = pickerLayout,
                man_pages = pickerLayout,
                marks = pickerLayout,
                colorscheme = pickerLayout,
                quickfix = pickerLayout,
                quickfixhistory = pickerLayout,
                loclist = pickerLayout,
                jumplist = pickerLayout,
                vim_options = pickerLayout,
                keymaps = pickerLayout,
                registers = pickerLayout,
                lsp_references = pickerLayout,
                lsp_workspace_symbols = pickerLayout,
                diagnostics = pickerLayout,
                lsp_definitions = pickerLayout,
                lsp_type_definitions = pickerLayout,
                lsp_implementations = pickerLayout,
                treesitter = pickerLayout,
            },
            extensions = {
                ["ui-select"] = {
                    require("telescope.themes").get_ivy(),
                },
                fzf = {},
            },
        })

        -- Enable Telescope extensions if they are installed
        pcall(require("telescope").load_extension, "fzf")
        pcall(require("telescope").load_extension, "ui-select")

        -- See `:help telescope.builtin`
    end,
}
