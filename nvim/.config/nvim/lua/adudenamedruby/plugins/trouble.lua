return {
	"folke/trouble.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = { "BufReadPre", "BufNewFile" },
	keys = {
		{ "<leader>et", "<cmd>Trouble quickfix toggle<cr>", { desc = "open a quickfix" } },
	},

	config = function()
		require("trouble").setup({
			auto_open = false,
			auto_close = false,
			auto_preview = true,
			auto_jump = false,
			mode = "quickfix",
			severity = vim.diagnostic.severity.WARN,
			cycle_results = false,
		})

		vim.api.nvim_create_autocmd("User", {
			pattern = { "XcodebuildBuildFinished", "XcodebuildTestsFinished" },
			callback = function(event)
				if event.data.cancelled then
					return
				end

				if event.data.success then
					require("trouble").close()
				elseif not event.data.failedCount or event.data.failedCount > 0 then
					if next(vim.fn.getqflist()) then
						require("trouble").open("quickfix")
					else
						require("trouble").close()
					end

					require("trouble").refresh()
				end
			end,
		})
	end,
}
