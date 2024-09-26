return {
	"lukas-reineke/indent-blankline.nvim",
	main = "ibl",
	config = function()
		require("ibl").setup({
			indent = {
				char = "▏",
			},
			scope = {
				show_start = false,
				show_end = false,
				show_exact_scope = false,
			},
			exclude = {
				filetypes = {
					"help",
					"startify",
					"dashboard",
					"packer",
					"lazy",
					"neogitstatus",
					"NvimTree",
					"Trouble",
				},
			},
		})

		KMap("<leader>ti", "<cmd>IBLToggle<CR>", "toggle indent-blankline")
		KMap("<leader>tI", "<cmd>IBLToggleScope<CR>", "toggle indent-blankline scope")
	end,
}
