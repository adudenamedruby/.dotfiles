return {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = function()
        local harpoon = require("harpoon")
        local U = require("adudenamedruby.core.utils")
        return {
            -- Add/Replace
            U.PLMap("haa", function()
                harpoon:list():add()
            end, "Add current file"),
            U.PLMap("hah", function()
                harpoon:list():replace_at(1)
            end, "Replace slot 1 (h)"),
            U.PLMap("hat", function()
                harpoon:list():replace_at(2)
            end, "Replace slot 2 (t)"),
            U.PLMap("han", function()
                harpoon:list():replace_at(3)
            end, "Replace slot 3 (n)"),
            U.PLMap("has", function()
                harpoon:list():replace_at(4)
            end, "Replace slot 4 (s)"),

            -- Remove/Clear
            U.PLMap("hrr", function()
                harpoon:list():remove()
            end, "Remove current file"),
            U.PLMap("hrc", function()
                harpoon:list():clear()
            end, "Clear all"),
            U.PLMap("hrh", function()
                harpoon:list():remove_at(1)
            end, "Remove slot 1"),
            U.PLMap("hrt", function()
                harpoon:list():remove_at(2)
            end, "Remove slot 2"),
            U.PLMap("hrn", function()
                harpoon:list():remove_at(3)
            end, "Remove slot 3"),
            U.PLMap("hrs", function()
                harpoon:list():remove_at(4)
            end, "Remove slot 4"),

            -- UI / Jump
            U.PLMap("hv", function()
                -- harpoon2 UI call (works if you’ve set it up this way)
                harpoon.ui:toggle_quick_menu(harpoon:list())
                -- if using the split modules style:
                -- require("harpoon.ui").toggle_quick_menu(harpoon:list())
            end, "Open Harpoon list"),

            U.PLMap("hh", function()
                harpoon:list():select(1)
            end, "Go to slot 1 (h)"),
            U.PLMap("ht", function()
                harpoon:list():select(2)
            end, "Go to slot 2 (t)"),
            U.PLMap("hn", function()
                harpoon:list():select(3)
            end, "Go to slot 3 (n)"),
            U.PLMap("hs", function()
                harpoon:list():select(4)
            end, "Go to slot 4 (s)"),
        }
    end,
    config = function()
        local harpoon = require("harpoon")
        harpoon.setup({})

        local ok, wk = pcall(require, "which-key")
        if ok then
            wk.add({
                { "<leader>h", group = "Harpoon" },
                { "<leader>ha", group = "Add/Replace" },
                { "<leader>hr", group = "Remove/Clear" },
            })
        end
    end,
}
