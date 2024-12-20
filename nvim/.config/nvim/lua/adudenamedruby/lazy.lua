-- [[ Install `lazy.nvim` plugin manager ]]
--    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        error("Error cloning lazy.nvim:\n" .. out)
    end
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- [[ Configure and install plugins ]]
--
--  To check the current status of your plugins, run
--    :Lazy
--
--  You can press `?` in this menu for help. Use `:q` to close the window
--
--  To update plugins you can run
--    :Lazy update
require("lazy").setup({
    { import = "adudenamedruby.plugins" },
}, {
    checker = {
        enabled = true,
        notify = false,
    },
    change_detection = {
        notify = false,
    },
})

-- KEYMAPS
KMap("<leader>HL", "<cmd>Lazy<CR>", "open Lazy")

-- Set random colourscheme
require("adudenamedruby.plugins.colorscheme")

-- Randomly select a colorscheme
local colorschemes = {
    "catppuccin",
    "tokyonight-moon",
    "kanagawa",
    "carbonfox",
    "duskfox",
    "nightwolf",
}
local random_colorscheme = colorschemes[math.random(#colorschemes)]
vim.cmd("colorscheme " .. random_colorscheme)
