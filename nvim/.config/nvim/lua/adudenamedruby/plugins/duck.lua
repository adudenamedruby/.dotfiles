return {
    "tamton-aquib/duck.nvim",
    keys = function()
        local duck = require("duck")
        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("uDd", function()
                duck.hatch()
            end, "summon duck"),
            U.PLMap("uDc", function()
                duck.cook()
            end, "cook duck"),
            U.PLMap("uDa", function()
                duck.cook_all()
            end, "cook all ducks"),
        }
    end,
    config = function() end,
}
