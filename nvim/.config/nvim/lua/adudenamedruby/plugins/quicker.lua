return {
	"stevearc/quicker.nvim",
	event = "FileType qf",
	config = function()
		dmap("<leader>tq", function()
			require("quicker").toggle()
		end, "toggle quickfix")

		dmap("<leader>tl", function()
			require("quicker").toggle({ loclist = true })
		end, "toggle loclist")

		require("quicker").setup({
			keys = {
				{
					">",
					function()
						require("quicker").expand({ before = 2, after = 2, add_to_existing = true })
					end,
					desc = "Expand quickfix context",
				},
				{
					"<",
					function()
						require("quicker").collapse()
					end,
					desc = "Collapse quickfix context",
				},
			},
		})
	end,
}
