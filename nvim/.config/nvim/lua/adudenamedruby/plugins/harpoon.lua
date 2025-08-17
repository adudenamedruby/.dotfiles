return {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = function()
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMapGroup("h", "harpoon"),
            U.PLMap("ha", "", "add to harpoon"),
            U.PLMap("haa", function()
                harpoon:list():add()
            end, "add to harpoon"),
            U.PLMap("hah", function()
                harpoon:list():replace_at(1)
            end, "add to h"),
            U.PLMap("hat", function()
                harpoon:list():replace_at(2)
            end, "add to t"),
            U.PLMap("han", function()
                harpoon:list():replace_at(3)
            end, "add to n"),
            U.PLMap("has", function()
                harpoon:list():replace_at(4)
            end, "add to s"),

            U.PLMap("hr", "", "remove"),
            U.PLMap("hrr", function()
                harpoon:list():remove()
            end, "remove current file"),
            U.PLMap("hrc", function()
                harpoon:list():clear()
            end, "clear harpoon"),
            U.PLMap("hrh", function()
                harpoon:list():remove_at(1)
            end, "clear h"),
            U.PLMap("hrt", function()
                harpoon:list():remove_at(2)
            end, "clear t"),
            U.PLMap("hrn", function()
                harpoon:list():remove_at(3)
            end, "clear n"),
            U.PLMap("hrs", function()
                harpoon:list():remove_at(4)
            end, "clear s"),

            -- U.PLMap("hv", function()
            --     -- toggle_telescope(harpoon:list())
            --     harpoon.ui:toggle_quick_menu(harpoon:list())
            -- end, "open harpoon list"),

            U.PLMap("hh", function()
                harpoon:list():select(1)
            end, "go to h"),
            U.PLMap("ht", function()
                harpoon:list():select(2)
            end, "go to t"),
            U.PLMap("hn", function()
                harpoon:list():select(3)
            end, "go to n"),
            U.PLMap("hs", function()
                harpoon:list():select(4)
            end, "go to s"),
        }
    end,
    config = function()
        local harpoon = require("harpoon")
        harpoon.setup({})
    end,
}
