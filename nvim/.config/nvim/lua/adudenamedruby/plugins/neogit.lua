return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"sindrets/diffview.nvim",
		"nvim-telescope/telescope.nvim",
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
