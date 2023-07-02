local status_ok, configs = pcall(require, "nvim-treesitter.configs")
if not status_ok then
    return
end

configs.setup {
    -- A list of parser names, or "all" (the five listed parsers should always be installed)
    ensure_installed = "all",

    -- Install parsers synchronously (only applied to `ensure_installed`)
    sync_install = false,

    -- Automatically install missing parsers when entering buffer
    -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
    auto_install = true,

    autopairs = {
        enable = true,
    },

    context_commentstring = {
        enable = true,
        enable_autocmd = false,
    },

    ignore_install = { "" }, -- List of parsers to ignore installing

    indent = { enable = true, disable = { "yaml" } },

    highlight = {
        enable = true,
        disable = { "" }, -- list of language that will be disabled
        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        -- Instead of true it can also be a list of languages
        additional_vim_regex_highlighting = false,
    },
}
