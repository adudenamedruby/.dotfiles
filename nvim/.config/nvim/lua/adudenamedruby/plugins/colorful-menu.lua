return {
    "xzbdmw/colorful-menu.nvim",
    config = function()
        -- You don't need to set these options.
        require("colorful-menu").setup({
            ft = {
                lua = {
                    -- Maybe you want to dim arguments a bit.
                    auguments_hl = "@comment",
                },
                typescript = {
                    -- Or "vtsls", their information is different, so we
                    -- need to know in advance.
                    ls = "typescript-language-server",
                    extra_info_hl = "@comment",
                },
                rust = {
                    -- such as (as Iterator), (use std::io).
                    extra_info_hl = "@comment",
                },
                c = {
                    -- such as "From <stdio.h>"
                    extra_info_hl = "@comment",
                },

                -- If we should try to highlight "not supported" languages
                fallback = true,
            },
            -- If the built-in logic fails to find a suitable highlight group,
            -- this highlight is applied to the label.
            fallback_highlight = "@variable",
            -- If provided, the plugin truncates the final displayed text to
            -- this width (measured in display cells). Any highlights that extend
            -- beyond the truncation point are ignored. Default 60.
            max_width = 60,
        })
        require("blink.cmp").setup({
            completion = {
                menu = {
                    draw = {
                        components = {
                            label = {
                                width = { fill = true, max = 60 },
                                text = function(ctx)
                                    local highlights_info =
                                        require("colorful-menu").highlights(ctx.item, vim.bo.filetype)
                                    if highlights_info ~= nil then
                                        return highlights_info.text
                                    else
                                        return ctx.label
                                    end
                                end,
                                highlight = function(ctx)
                                    local highlights_info =
                                        require("colorful-menu").highlights(ctx.item, vim.bo.filetype)
                                    local highlights = {}
                                    if highlights_info ~= nil then
                                        for _, info in ipairs(highlights_info.highlights) do
                                            table.insert(highlights, {
                                                info.range[1],
                                                info.range[2],
                                                group = ctx.deprecated and "BlinkCmpLabelDeprecated" or info[1],
                                            })
                                        end
                                    end
                                    for _, idx in ipairs(ctx.label_matched_indices) do
                                        table.insert(highlights, { idx, idx + 1, group = "BlinkCmpLabelMatch" })
                                    end
                                    return highlights
                                end,
                            },
                        },
                    },
                },
            },
        })
    end,
}
