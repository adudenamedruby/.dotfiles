local M = {}

function M.GMap(keys, func, desc, mode, expr)
    vim.keymap.set(mode or "n", keys, func, {
        expr = expr or false,
        desc = desc or "",
        noremap = true,
        silent = true,
    })
end

function M.GLMap(keys, func, desc, mode, expr)
    M.GMap("<leader>" .. keys, func, desc, mode, expr)
end

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
