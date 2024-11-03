return {
    "adudenamedruby/scratch-buf",
    lazy = true,
    cmd = {
        "ScratchVSplit",
        "ScratchHSplit",
    },
    opts = {},
    KMap("<leader>bs", "<cmd>ScratchVSplit<cr>", "scratch buffer (vertical)"),
    KMap("<leader>bS", "<cmd>ScratchHSplit<cr>", "scratch buffer (horizontal)"),
}
