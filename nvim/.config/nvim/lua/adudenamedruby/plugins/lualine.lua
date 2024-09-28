return {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        local lualine = require("lualine")

        local function xcodebuild_device()
            if vim.g.xcodebuild_platform == "macOS" then
                return " macOS"
            end

            if vim.g.xcodebuild_os then
                return " " .. vim.g.xcodebuild_device_name .. " (" .. vim.g.xcodebuild_os .. ")"
            end

            return " " .. vim.g.xcodebuild_device_name
        end

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
            -- symbols = {
            -- 	error = function(count)
            -- 		return count > 0 and " " .. count or ""
            -- 	end,
            -- 	warn = function(count)
            -- 		return count > 0 and " " .. count or ""
            -- 	end,
            -- 	info = function(count)
            -- 		return count > 0 and " " .. count or ""
            -- 	end,
            -- 	hint = function(count)
            -- 		return count > 0 and " " .. count or ""
            -- 	end,
            -- },
            colored = true,
            update_in_insert = false,
            always_visible = true,
        }

        local diff = {
            "diff",
            colored = true,
            symbols = { added = " ", modified = " ", removed = " " }, -- changes diff symbols
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
                    { "' ' .. vim.g.xcodebuild_last_status", color = { fg = "#a6e3a1" } },
                    -- { "'󰙨 ' .. vim.g.xcodebuild_test_plan", color = { fg = "#a6e3a1", bg = "#161622" } },
                    { xcodebuild_device, color = { fg = "#f9e2af", bg = "#161622" } },
                },
                lualine_y = { diff, "branch" },
                lualine_z = { "location" },
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
