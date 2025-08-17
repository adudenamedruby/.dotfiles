-- Function: KMap
-- Description: Binds a specified keymap to an action with noremap and silent mode
-- Parameters:
--   mode - The modes in which you want the keybind to appear in. Default value = "n"
--   keys - The keys for the keybind
--   func - What to do when pressing that keybind
--   desc - A description to show up in which-key. Default value = ""
local function Map(keys, func, desc, mode, expr)
    return {
        keys,
        func,
        mode = mode or "n",
        desc = desc or "",
        expr = expr or false,
        silent = true,
        noremap = true,
    }
end

local function WMap(keys, func, desc, mode, expr)
    return Map("<leader>" .. keys, func, desc, mode, expr)
end

return {
    Map = Map,
    WMap = WMap,
}
