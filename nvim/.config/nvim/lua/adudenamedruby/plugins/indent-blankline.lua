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

			vim.keymap.set("n", "<leader>ti", "<cmd>IBLToggle<CR>", { desc = "toggle indent-blankline" }),
		})
	end,
}
