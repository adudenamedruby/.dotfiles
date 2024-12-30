local wk = require("which-key")

-- Function: KMap
-- Description: Binds a specified keymap to an action with noremap and silent mode
-- Parameters:
--   mode - The modes in which you want the keybind to appear in. Default value = "n"
--   keys - The keys for the keybind
--   func - What to do when pressing that keybind
--   desc - A description to show up in which-key. Default value = ""
KMap = function(keys, func, desc, mode, expr)
    mode = mode or "n"
    desc = desc or ""
    expr = expr or false

    if expr then
        vim.keymap.set(mode, keys, func, { expr = true, desc = desc, noremap = true, silent = true })
    else
        vim.keymap.set(mode, keys, func, { desc = desc, noremap = true, silent = true })
    end
end

WKMap = function(keys, func, desc, mode, expr)
    KMap("<leader>" .. keys, func, desc, mode, expr)
end

WKMapGroup = function(lhs, group_desc)
    local spec = {
        {
            "<leader>" .. lhs,
            group = group_desc,
        },
    }
    wk.add(spec)
end
