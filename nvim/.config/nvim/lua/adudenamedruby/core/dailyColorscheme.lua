local colorschemes = {
    "ember",
    "catppuccin",
    "tokyonight-moon",
    "kanagawa",
    "carbonfox",
    "duskfox",
    "mfd-amber",
    "kanso",
    "gruvbox-material",
    "miasma",
    "thorn",
}

-- Map colorscheme names to their lazy.nvim plugin names
local scheme_to_plugin = {
    ["ember"] = "ember",
    ["catppuccin"] = "catppuccin",
    ["tokyonight-moon"] = "tokyonight",
    ["kanagawa"] = "kanagawa",
    ["carbonfox"] = "nightfox.nvim",
    ["duskfox"] = "nightfox.nvim",
    ["mfd-amber"] = "mfd.nvim",
    ["gruvbox-material"] = "gruvbox-material",
    ["kanso"] = "kanso.nvim",
    ["miasma"] = "miasma.nvim",
    ["thorn"] = "thorn.nvim",
}

local M = {}

function M.set_daily_colorscheme()
    -- Stable per-day index: epoch-day count at local midnight, modulo list size.
    -- Increments by exactly 1 each local day, so themes cycle in list order.
    local t = os.date("*t")
    t.hour, t.min, t.sec = 0, 0, 0
    local day_count = math.floor(os.time(t) / 86400)
    local chosen_scheme = colorschemes[(day_count % #colorschemes) + 1]

    local plugin_name = scheme_to_plugin[chosen_scheme]
    if plugin_name then
        require("lazy").load({ plugins = { plugin_name } })
    end

    vim.cmd("colorscheme " .. chosen_scheme)
end

return M
