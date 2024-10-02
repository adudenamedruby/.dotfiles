return {
    {
        "catppuccin/nvim",
        name = "catppuccin",
        lazy = false,
        priority = 1000,
        config = function()
            require("catppuccin").setup({
                flavour = "mocha", -- latte, frappe, macchiato, mocha
                background = { -- :h background
                    light = "latte",
                    dark = "mocha",
                },
                transparent_background = false, -- disables setting the background color.
                show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
                term_colors = false, -- sets terminal colors (e.g. `g:terminal_color_0`)
                dim_inactive = {
                    enabled = false, -- dims the background color of inactive window
                    shade = "dark",
                    percentage = 0.15, -- percentage of the shade to apply to the inactive window
                },
                no_italic = false, -- Force no italic
                no_bold = false, -- Force no bold
                no_underline = false, -- Force no underline
                styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
                    comments = { "italic" }, -- Change the style of comments
                    conditionals = { "italic" },
                    loops = {},
                    functions = {},
                    keywords = {},
                    strings = {},
                    variables = {},
                    numbers = {},
                    booleans = {},
                    properties = {},
                    types = {},
                    operators = {},
                    -- miscs = {}, -- Uncomment to turn off hard-coded styles
                },
                color_overrides = {},
                default_integrations = true,
                integrations = {
                    cmp = true,
                    gitsigns = true,
                    nvimtree = true,
                    treesitter = true,
                    notify = false,
                    mini = {
                        enabled = true,
                        indentscope_color = "",
                    },
                    -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
                },
                custom_highlights = function(colors)
                    local searchActive = { bg = colors.red, fg = "#181825" }
                    local searchInactive = { bg = colors.peach, fg = "#000000" }

                    return {
                        CursorLineNr = { fg = colors.flamingo },
                        Search = searchInactive,
                        IncSearch = searchActive,
                        EndOfBuffer = { fg = colors.flamingo },
                        WinSeparator = { fg = colors.surface0, bg = colors.surface0 },
                    }
                end,
            })
        end,
    },
    {
        "folke/tokyonight.nvim",
        name = "tokyonight",
        lazy = false,
        priority = 1000,
        config = function()
            require("tokyonight").setup({
                style = "moon", -- The theme comes in three styles, `storm`, a darker variant `night` and `day`
                light_style = "day", -- The theme is used when the background is set to light
                transparent = false, -- Enable this to disable setting the background color
                terminal_colors = true, -- Configure the colors used when opening a `:terminal` in Neovim
                styles = {
                    -- Style to be applied to different syntax groups
                    -- Value is any valid attr-list value for `:help nvim_set_hl`
                    comments = { italic = true },
                    keywords = { italic = true },
                    functions = {},
                    variables = {},
                    -- Background styles. Can be "dark", "transparent" or "normal"
                    sidebars = "dark", -- style for sidebars, see below
                    floats = "dark", -- style for floating windows
                },
                day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
                dim_inactive = false, -- dims inactive windows
                lualine_bold = false, -- When `true`, section headers in the lualine theme will be bold

                --- You can override specific color groups to use other groups or a hex color
                --- function will be called with a ColorScheme table
                ---@param colors ColorScheme
                on_colors = function(colors) end,

                --- You can override specific highlights to use other groups or a hex color
                --- function will be called with a Highlights and ColorScheme table
                ---@param highlights tokyonight.Highlights
                ---@param colors ColorScheme
                on_highlights = function(highlights, colors) end,

                cache = true, -- When set to true, the theme will be cached for better performance

                ---@type table<string, boolean|{enabled:boolean}>
                plugins = {
                    -- enable all plugins when not using lazy.nvim
                    -- set to false to manually enable/disable plugins
                    all = package.loaded.lazy == nil,
                    -- uses your plugin manager to automatically enable needed plugins
                    -- currently only lazy.nvim is supported
                    auto = true,
                    -- add any plugins here that you want to enable
                    -- for all possible plugins, see:
                    --   * https://github.com/folke/tokyonight.nvim/tree/main/lua/tokyonight/groups
                    -- telescope = true,
                },
            })
        end,
    },
    {
        "rebelot/kanagawa.nvim",
        name = "kanagawa",
        lazy = false,
        priority = 1000,
        config = function()
            require("kanagawa").setup({
                compile = false, -- enable compiling the colorscheme
                undercurl = true, -- enable undercurls
                commentStyle = { italic = true },
                functionStyle = {},
                keywordStyle = { italic = true },
                statementStyle = { bold = true },
                typeStyle = {},
                transparent = false, -- do not set background color
                dimInactive = true, -- dim inactive window `:h hl-NormalNC`
                terminalColors = true, -- define vim.g.terminal_color_{0,17}
                colors = { -- add/modify theme and palette colors
                    palette = {},
                    theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
                },
                overrides = function(colors) -- add/modify highlights
                    return {}
                end,
                theme = "wave", -- Load "wave" theme when 'background' option is not set
                background = { -- map the value of 'background' option to a theme
                    dark = "wave", -- try "dragon" !
                    light = "lotus",
                },
            })
        end,
    },
    {
        "rose-pine/neovim",
        name = "rose-pine",
        lazy = false,
        priority = 1000,
        config = function()
            require("rose-pine").setup({
                variant = "main", -- auto, main, moon, or dawn
                dark_variant = "main", -- main, moon, or dawn
                dim_inactive_windows = false,
                extend_background_behind_borders = true,

                enable = {
                    terminal = true,
                    legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
                    migrations = true, -- Handle deprecated options automatically
                },

                styles = {
                    bold = true,
                    italic = true,
                    transparency = false,
                },

                groups = {
                    border = "muted",
                    link = "iris",
                    panel = "surface",

                    error = "love",
                    hint = "iris",
                    info = "foam",
                    note = "pine",
                    todo = "rose",
                    warn = "gold",

                    git_add = "foam",
                    git_change = "rose",
                    git_delete = "love",
                    git_dirty = "rose",
                    git_ignore = "muted",
                    git_merge = "iris",
                    git_rename = "pine",
                    git_stage = "iris",
                    git_text = "rose",
                    git_untracked = "subtle",

                    h1 = "iris",
                    h2 = "foam",
                    h3 = "rose",
                    h4 = "gold",
                    h5 = "pine",
                    h6 = "foam",
                },

                palette = {
                    -- Override the builtin palette per variant
                    -- moon = {
                    --     base = '#18191a',
                    --     overlay = '#363738',
                    -- },
                },

                highlight_groups = {
                    -- Comment = { fg = "foam" },
                    -- VertSplit = { fg = "muted", bg = "muted" },
                },

                before_highlight = function(group, highlight, palette)
                    -- Disable all undercurls
                    -- if highlight.undercurl then
                    --     highlight.undercurl = false
                    -- end
                    --
                    -- Change palette colour
                    -- if highlight.fg == palette.pine then
                    --     highlight.fg = palette.foam
                    -- end
                end,
            })
        end,
    },
    {
        "EdenEast/nightfox.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            require("nightfox").setup({
                options = {
                    -- Compiled file's destination location
                    compile_path = vim.fn.stdpath("cache") .. "/nightfox",
                    compile_file_suffix = "_compiled", -- Compiled file suffix
                    transparent = false, -- Disable setting background
                    terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
                    dim_inactive = false, -- Non focused panes set to alternative background
                    module_default = true, -- Default enable value for modules
                    colorblind = {
                        enable = false, -- Enable colorblind support
                        simulate_only = false, -- Only show simulated colorblind colors and not diff shifted
                        severity = {
                            protan = 0, -- Severity [0,1] for protan (red)
                            deutan = 0, -- Severity [0,1] for deutan (green)
                            tritan = 0, -- Severity [0,1] for tritan (blue)
                        },
                    },
                    styles = { -- Style to be applied to different syntax groups
                        comments = "NONE", -- Value is any valid attr-list value `:help attr-list`
                        conditionals = "NONE",
                        constants = "NONE",
                        functions = "NONE",
                        keywords = "NONE",
                        numbers = "NONE",
                        operators = "NONE",
                        strings = "NONE",
                        types = "NONE",
                        variables = "NONE",
                    },
                    inverse = { -- Inverse highlight for different types
                        match_paren = false,
                        visual = false,
                        search = false,
                    },
                    modules = { -- List of various plugins and additional options
                        -- ...
                    },
                },
                palettes = {},
                specs = {},
                groups = {},
            })
        end,
    },
    {
        "neanias/everforest-nvim",
        name = "everforest",
        version = false,
        lazy = false,
        priority = 1000,
        config = function()
            require("everforest").setup({
                -----Controls the "hardness" of the background. Options are "soft", "medium" or "hard".
                background = "hard",
                ---How much of the background should be transparent. 2 will have more UI
                ---components be transparent (e.g. status line background)
                transparent_background_level = 0,
                ---Whether italics should be used for keywords and more.
                italics = false,
                ---Disable italic fonts for comments. Comments are in italics by default, set
                ---this to `true` to make them _not_ italic!
                disable_italic_comments = false,
                ---By default, the colour of the sign column background is the same as the as normal text
                ---background, but you can use a grey background by setting this to `"grey"`.
                sign_column_background = "none",
                ---The contrast of line numbers, indent lines, etc. Options are `"high"` or `low`
                ui_contrast = "low",
                ---Dim inactive windows. Only works in Neovim. Can look a bit weird with Telescope.
                ---
                ---When this option is used in conjunction with show_eob set to `false`, the
                ---end of the buffer will only be hidden inside the active window. Inside
                ---inactive windows, the end of buffer filler characters will be visible in
                ---dimmed symbols. This is due to the way Vim and Neovim handle `EndOfBuffer`.
                dim_inactive_windows = false,
                ---Some plugins support highlighting error/warning/info/hint texts, by
                ---default these texts are only underlined, but you can use this option to
                ---also highlight the background of them.
                diagnostic_text_highlight = false,
                ---Which colour the diagnostic text should be. Options are `"grey"` or `"coloured"` (default)
                diagnostic_virtual_text = "coloured",
                ---Some plugins support highlighting error/warning/info/hint lines, but this
                ---feature is disabled by default in this colour scheme.
                diagnostic_line_highlight = false,
                ---By default, this color scheme won't colour the foreground of |spell|, instead
                ---colored under curls will be used. If you also want to colour the foreground,
                ---set this option to `true`.
                spell_foreground = false,
                ---Whether to show the EndOfBuffer highlight.
                show_eob = true,
                ---Style used to make floating windows stand out from other windows. `"bright"`
                ---makes the background of these windows lighter than |hl-Normal|, whereas
                ---`"dim"` makes it darker.
                ---
                ---Floating windows include for instance diagnostic pop-ups, scrollable
                ---documentation windows from completion engines, overlay windows from
                ---installers, etc.
                ---
                ---NB: This is only significant for dark backgrounds as the light palettes
                ---have the same colour for both values in the switch.
                float_style = "bright",
                ---Inlay hints are special markers that are displayed inline with the code to
                ---provide you with additional information. You can use this option to customize
                ---the background color of inlay hints.
                ---
                ---Options are `"none"` or `"dimmed"`.
                inlay_hints_background = "none",
            })
        end,
    },
    {
        "olivercederborg/poimandres.nvim",
        name = "poimandres",
        lazy = false,
        priority = 1000,
        config = function()
            require("poimandres").setup({
                -- leave this setup function empty for default config
                -- or refer to the configuration section
                -- for configuration options
            })
        end,
    },
}
