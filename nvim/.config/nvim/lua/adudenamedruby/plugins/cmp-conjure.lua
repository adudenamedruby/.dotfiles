return {
    "PaterJason/cmp-conjure",
    lazy = true,
    config = function()
        -- local cmp = require("cmp")
        local cmp = require("blink")
        local config = cmp.get_config()
        table.insert(config.sources, { name = "conjure" })
        return cmp.setup(config)
    end,
}
