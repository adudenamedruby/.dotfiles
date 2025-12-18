local M = {}

--- Create a global keymap
--- Sets noremap and silent by default
---@param keys string The key sequence (e.g., "gd", "<C-n>")
---@param func string|function The command or function to execute
---@param desc string Description for which-key and :map
---@param mode? string|table Mode(s) to map in ("n", "v", {"n", "v"}, etc.). Defaults to "n"
---@param expr? boolean Whether the mapping is an expression mapping. Defaults to false
function M.GMap(keys, func, desc, mode, expr)
    vim.keymap.set(mode or "n", keys, func, {
        expr = expr or false,
        desc = desc or "",
        noremap = true,
        silent = true,
    })
end

--- Create a global keymap with <leader> prefix
--- Convenience wrapper around GMap that prepends "<leader>"
---@param keys string The key sequence after <leader> (e.g., "ff", "ca")
---@param func string|function The command or function to execute
---@param desc string Description for which-key and :map
---@param mode? string|table Mode(s) to map in. Defaults to "n"
---@param expr? boolean Whether the mapping is an expression mapping. Defaults to false
function M.GLMap(keys, func, desc, mode, expr)
    M.GMap("<leader>" .. keys, func, desc, mode, expr)
end

--- Create a plugin keymap spec for lazy.nvim
--- Returns a table in lazy.nvim's keys spec format for lazy-loading
---@param keys string The key sequence (e.g., "gd", "<C-n>")
---@param func string|function The command or function to execute
---@param desc string Description for which-key and :map
---@param mode? string|table Mode(s) to map in. Defaults to "n"
---@param expr? boolean Whether the mapping is an expression mapping. Defaults to false
---@return table # Lazy.nvim keys spec table
function M.PMap(keys, func, desc, mode, expr)
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

--- Create a plugin keymap spec with <leader> prefix for lazy.nvim
--- Returns a table in lazy.nvim's keys spec format with "<leader>" prepended
--- Use this in plugin configs' keys function to enable lazy-loading on keypress
---@param keys string The key sequence after <leader> (e.g., "ff", "ca")
---@param func string|function The command or function to execute
---@param desc string Description for which-key and :map
---@param mode? string|table Mode(s) to map in. Defaults to "n"
---@param expr? boolean Whether the mapping is an expression mapping. Defaults to false
---@return table # Lazy.nvim keys spec table with <leader> prefix
function M.PLMap(keys, func, desc, mode, expr)
    return M.PMap("<leader>" .. keys, func, desc, mode, expr)
end

-- function M.PLMapGroup(lhs, group_desc)
--     return { "<leader>" .. lhs, group = group_desc }
-- end
-- function M.PLMapGroup(lhs, group_desc)
--     local ok, wk = pcall(require, "which-key")
--     if ok then
--         wk.add({ { "<leader>" .. lhs, group = group_desc } })
--     end
-- end
return M
