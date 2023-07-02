-- This file can be loaded by calling `lua require("plugins")` from your init.vim

local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_BOOTSTRAP = fn.system {
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    }
    print "Installing packer close and reopen Neovim..."
    vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
    augroup packer_user_config
        autocmd!
        autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
]]

-- Use a protected call so we don"t error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

-- Have packer use a popup window
packer.init {
    display = {
        open_fn = function()
            return require("packer.util").float { border = "rounded" }
        end,
    },
}

-- Plugins go here!
return packer.startup(function(use)
    -- Packer managens itself
    use "wbthomason/packer.nvim" -- Packer can manage itself
    -- Widely Used dependencies
    use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
    use "nvim-lua/plenary.nvim" -- Useful lua functions used ny lots of plugins

    -- Theme!
    use { "catppuccin/nvim", as = "catppuccin" }
    -- vim.cmd("colorscheme catppuccin-mocha")

    -- Telescope
    use { "nvim-telescope/telescope.nvim", tag = "*" }
    use "nvim-telescope/telescope-media-files.nvim"

    -- Autocompletion plugins
    use "hrsh7th/nvim-cmp" -- The completion plugin
    use "hrsh7th/cmp-buffer" -- buffer completions
    use "hrsh7th/cmp-path" -- path completions
    use "hrsh7th/cmp-cmdline" -- cmdline completions
    use "hrsh7th/cmp-nvim-lsp" -- lsp completions
    use "hrsh7th/cmp-nvim-lua" -- lua completions
    use "saadparwaiz1/cmp_luasnip" -- snippet completions

    -- Snippets
    use "L3MON4D3/LuaSnip" --snippet engine
    use "rafamadriz/friendly-snippets" -- a bunch of snippets to use

    -- LSP
    use "neovim/nvim-lspconfig" -- enable LSP
    use {
        "williamboman/mason.nvim", -- simple to use language server installer
        run = ":MasonUpdate" -- :MasonUpdate updates registry contents
    }
    use "williamboman/mason-lspconfig.nvim" -- simple to use language server installer
    use "jose-elias-alvarez/null-ls.nvim" -- LSP diagnostics and code actions

    -- Treesitter
    use {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
    }
    use "JoosepAlviste/nvim-ts-context-commentstring"

    -- Autopairs
    use "windwp/nvim-autopairs" -- Autopairs, integrates with both cmp and treesitter

    -- Comments
    use "numToStr/Comment.nvim" -- Easily comment stuff

     -- Git
    use "lewis6991/gitsigns.nvim"

    -- Nvim Tree
    use 'kyazdani42/nvim-web-devicons'
    use 'kyazdani42/nvim-tree.lua'

    -- Bufferline
    use { "akinsho/bufferline.nvim", tag = "*" }
    use "moll/vim-bbye"

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end

end)
