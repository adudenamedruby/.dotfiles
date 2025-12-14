return {
    -- Collection of various small independent plugins/modules
    "echasnovski/mini.nvim",
    keys = function()
        local mini = require("mini.jump2d")
        local U = require("adudenamedruby.core.utils")

        return {
            U.PLMap("jc", function()
                mini.start()
            end, "jump to characters"),
        }
    end,
    config = function()
        -- Better Around/Inside textobjects
        --
        -- Examples:
        --  - va)  - [V]isually select [A]round [)]paren
        --  - yinq - [Y]ank [I]nside [N]ext [Q]uote
        --  - ci'  - [C]hange [I]nside [']quote
        --  n or l - next or last functionality
        require("mini.ai").setup({ n_lines = 500 })

        -- this is very useful, by hitting `sj` you can split arguments into new lines (ctrl+m in Xcode)
        require("mini.splitjoin").setup({
            mappings = {
                toggle = "<C-m>",
                split = "",
                join = "",
            },
        })

        require("mini.pairs").setup()

        require("mini.jump2d").setup()

        -- Add/delete/replace surroundings (brackets, quotes, etc.)
        --
        -- testword
        -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
        -- - sd'   - [S]urround [D]elete [']quotes
        -- - sr)'  - [S]urround [R]eplace [)] [']
        require("mini.surround").setup()
    end,
}
