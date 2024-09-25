return {
	"ThePrimeagen/harpoon",
	branch = "harpoon2",
	dependencies = { "nvim-lua/plenary.nvim" },
	config = function()
		local harpoon = require("harpoon")
		harpoon.setup({})
		-- local conf = require("telescope.config").values
		-- local function toggle_telescope(harpoon_files)
		-- 	local file_paths = {}
		-- 	for _, item in ipairs(harpoon_files.items) do
		-- 		table.insert(file_paths, item.value)
		-- 	end
		--
		-- 	require("telescope.pickers")
		-- 		.new({}, {
		-- 			prompt_title = "Harpoon",
		-- 			finder = require("telescope.finders").new_table({
		-- 				results = file_paths,
		-- 			}),
		-- 			previewer = conf.file_previewer({}),
		-- 			sorter = conf.generic_sorter({}),
		-- 		})
		-- 		:find()
		-- end

		-- Keymaps
		vim.keymap.set("n", "<leader>ha", function()
			harpoon:list():add()
		end, { desc = "add to harpoon" })
		vim.keymap.set("n", "<leader>hd", function()
			harpoon:list():remove()
		end, { desc = "remove from harpoon" })
		vim.keymap.set("n", "<leader>hv", function()
			-- toggle_telescope(harpoon:list())
			harpoon.ui:toggle_quick_menu(harpoon:list())
		end, { desc = "open harpoon list" })

		vim.keymap.set("n", "<leader>hh", function()
			harpoon:list():select(1)
		end, { desc = "goto harpoon 1" })
		vim.keymap.set("n", "<leader>ht", function()
			harpoon:list():select(2)
		end, { desc = "goto harpoon 2" })
		vim.keymap.set("n", "<leader>hs", function()
			harpoon:list():select(3)
		end, { desc = "goto harpoon 3" })

		-- Toggle previous & next buffers stored within Harpoon list
		vim.keymap.set("n", "<leader>hp", function()
			harpoon:list():prev()
		end, { desc = "harpoon previous" })
		vim.keymap.set("n", "<leader>hn", function()
			harpoon:list():next()
		end, { desc = "harpoon next" })
	end,
}
