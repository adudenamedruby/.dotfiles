local colorschemes = {
    "ember",
    "catppuccin",
    "tokyonight-moon",
    "kanagawa",
    "carbonfox",
    "duskfox",
    "mfd-amber",
    "mfd-hud",
    "mfd-flir-fusion",
    "mfd-nerv",
    "mithrandir",
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
    ["mfd-hud"] = "mfd.nvim",
    ["mfd-flir-fusion"] = "mfd.nvim",
    ["mfd-nerv"] = "mfd.nvim",
    ["mithrandir"] = "mithrandir.nvim",
}

local function pick_new_scheme(old_scheme)
    local new_scheme = colorschemes[math.random(#colorschemes)]
    while new_scheme == old_scheme do
        new_scheme = colorschemes[math.random(#colorschemes)]
    end
    return new_scheme
end

local M = {}

function M.set_daily_colorscheme()
    -- Where to persist scheme & date info: ~/.cache/nvim/daily_colorscheme
    local daily_file = vim.fn.stdpath("cache") .. "/daily_colorscheme"
    local today = os.date("%Y-%m-%d")
    local chosen_scheme = nil

    local file = io.open(daily_file, "r")
    if file then
        local content = file:read("*all")
        file:close()

        local stored_date, stored_scheme = content:match("^(%d%d%d%d%-%d%d%-%d%d)\n(.*)$")

        if stored_date == today then
            chosen_scheme = stored_scheme
        else
            chosen_scheme = pick_new_scheme(stored_scheme)
            file = io.open(daily_file, "w")
            file:write(today .. "\n" .. chosen_scheme)
            file:close()
        end
    else
        chosen_scheme = pick_new_scheme(nil)
        file = io.open(daily_file, "w")
        file:write(today .. "\n" .. chosen_scheme)
        file:close()
    end

    -- Load only the needed colorscheme plugin
    local plugin_name = scheme_to_plugin[chosen_scheme]
    if plugin_name then
        require("lazy").load({ plugins = { plugin_name } })
    end

    vim.cmd("colorscheme " .. chosen_scheme)
end

return M
