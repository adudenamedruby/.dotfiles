return {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        local lualine = require("lualine")
        local harpoon_files = require("harpoon_files")

        local truncatedMode = {
            "mode",
            fmt = function(str)
                -- return " " .. str
                return " : " .. str:sub(1, 1)
            end,
        }

        local hide_in_width = function()
            return vim.fn.winwidth(0) > 100
        end

        local diagnostics = {
            "diagnostics",
            sources = { "nvim_diagnostic" },
            sections = { "error", "warn", "info", "hint" },
            symbols = { error = " ", warn = " ", info = " ", hint = " " },
            -- fmt = function(str)
            -- 	if tonumber(str:match("%d+")) == 0 then
            -- 		return ""
            -- 	end
            -- 	return str
            -- end,
            colored = true,
            update_in_insert = false,
            always_visible = false,
        }

        local diff = {
            "diff",
            colored = true,
            symbols = { added = " ", modified = "∆ ", removed = "- " }, -- changes diff symbols
            cond = hide_in_width,
        }

        lualine.setup({
            options = {
                icons_enabled = true,
                globalstatus = true,
                theme = "auto",
                symbols = {
                    alternate_file = "#",
                    directory = "",
                    readonly = "",
                    unnamed = "[No Name]",
                    newfile = "[New]",
                },
                disabled_buftypes = { "quickfix", "prompt", "neo-tree" },
                -- Some useful glyphs:
                -- https://www.nerdfonts.com/cheat-sheet
                --          
                section_separators = { left = "", right = "" },
                component_separators = { left = "", right = "" },
            },
            sections = {
                lualine_a = { truncatedMode },
                lualine_b = { "filename" },
                lualine_c = {
                    diagnostics,
                    {
                        "searchcount",
                        maxcount = 999,
                        timeout = 500,
                    },
                },
                lualine_x = {
                    { harpoon_files.lualine_component },
                },
                lualine_y = { diff, "branch" },
                lualine_z = { "filetype", "location", "progress" },
            },
            inactive_sections = {
                lualine_a = {},
                lualine_b = {},
                lualine_c = { "filename" },
                lualine_x = {},
                lualine_y = {},
                lualine_z = {},
            },
            extensions = { "nvim-dap-ui", "quickfix", "trouble", "nvim-tree", "oil", "lazy", "mason" },
        })
    end,
}
