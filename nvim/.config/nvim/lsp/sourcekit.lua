return {
    capabilities = {
        workspace = {
            didChangeWatchedFiles = {
                dynamicRegistration = true,
            },
        },
    },
    filetypes = { "swift", "objective-c", "objective-cpp" },
    on_init = function(client)
        -- HACK: to fix some issues with LSP
        -- more details: https://github.com/neovim/neovim/issues/19237#issuecomment-2237037154
        client.offset_encoding = "utf-8"
    end,
}
