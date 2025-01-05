return {
    -- Essential tools for working with rust
    "mrcjkb/rustaceanvim",
    version = "^5", -- Recommended
    lazy = false, -- This plugin is already lazy
    ["rust-analyzer"] = {
        cargo = {
            allFeatures = true,
        },
    },
    config = function()
        vim.g.rustaceanvim = {
            tools = {
                float_win_config = {
                    auto_focus = true,
                },
            },
        }
    end,
}
