return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim", -- required
		"sindrets/diffview.nvim", -- optional - Diff integration

		"nvim-telescope/telescope.nvim", -- optional
	},
	config = true,
	keys = {
		{ "<leader>gs", "<cmd>Neogit<cr>", desc = "NeoGit" },
	},
}
