return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim", -- required
		"sindrets/diffview.nvim", -- optional - Diff integration

		"nvim-telescope/telescope.nvim", -- optional
	},
	config = true,
	opts = {
		mappings = {
			popup = {
				["F"] = "PullPopup",
				["p"] = false,
			},
		},
	},
	keys = {
		{ "<leader>gs", "<cmd>Neogit<cr>", desc = "NeoGit" },
	},
}
