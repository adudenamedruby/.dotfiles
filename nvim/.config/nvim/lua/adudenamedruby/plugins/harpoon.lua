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
		KMap("n", "<leader>ha", function()
			harpoon:list():add()
		end, "add to harpoon")
		KMap("n", "<leader>hd", function()
			harpoon:list():remove()
		end, "remove from harpoon")
		KMap("n", "<leader>hv", function()
			-- toggle_telescope(harpoon:list())
			harpoon.ui:toggle_quick_menu(harpoon:list())
		end, "open harpoon list")

		KMap("n", "<leader>hh", function()
			harpoon:list():select(1)
		end, "goto harpoon 1")
		KMap("n", "<leader>ht", function()
			harpoon:list():select(2)
		end, "goto harpoon 2")
		KMap("n", "<leader>hs", function()
			harpoon:list():select(3)
		end, "goto harpoon 3")

		-- Toggle previous & next buffers stored within Harpoon list
		KMap("n", "<leader>hp", function()
			harpoon:list():prev()
		end, "harpoon previous")
		KMap("n", "<leader>hn", function()
			harpoon:list():next()
		end, "harpoon next")
	end,
}
