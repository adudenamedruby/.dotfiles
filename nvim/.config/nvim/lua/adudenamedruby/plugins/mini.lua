return {
	-- Collection of various small independent plugins/modules
	"echasnovski/mini.nvim",
	config = function()
		-- Better Around/Inside textobjects
		--
		-- Examples:
		--  - va)  - [V]isually select [A]round [)]paren
		--  - yinq - [Y]ank [I]nside [N]ext [Q]uote
		--  - ci'  - [C]hange [I]nside [']quote
		require("mini.ai").setup({ n_lines = 500 })

		require("mini.jump2d").setup({
			mappings = {
				start_jumping = "<leader>uj",
			},
		})

		-- this is very useful, by hitting `sj` you can split arguments into new lines (ctrl+m in Xcode)
		require("mini.splitjoin").setup({
			mappings = {
				toggle = "<C-m>",
				split = "",
				join = "",
			},
		})

		require("mini.pairs").setup()

		-- Add/delete/replace surroundings (brackets, quotes, etc.)
		--
		-- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
		-- - sd'   - [S]urround [D]elete [']quotes
		-- - sr)'  - [S]urround [R]eplace [)] [']
		require("mini.surround").setup()
	end,
}
