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
		KMap("<leader>ha", "", "add to harpoon")
		KMap("<leader>haa", function()
			harpoon:list():add()
		end, "add to harpoon")
		KMap("<leader>hah", function()
			harpoon:list():replace_at(1)
		end, "add to harpoon at 1")
		KMap("<leader>hat", function()
			harpoon:list():replace_at(2)
		end, "add to harpoon at 2")
		KMap("<leader>han", function()
			harpoon:list():replace_at(4)
		end, "add to harpoon at 3")
		KMap("<leader>has", function()
			harpoon:list():replace_at(4)
		end, "add to harpoon at 4")

		KMap("<leader>hr", function()
			harpoon:list():remove()
		end, "remove from harpoon")
		KMap("<leader>hc", function()
			harpoon:list():clear()
		end, "clear harpoon")
		KMap("<leader>hv", function()
			-- toggle_telescope(harpoon:list())
			harpoon.ui:toggle_quick_menu(harpoon:list())
		end, "open harpoon list")

		KMap("<leader>hh", function()
			harpoon:list():select(1)
		end, "goto harpoon 1")
		KMap("<leader>ht", function()
			harpoon:list():select(2)
		end, "goto harpoon 2")
		KMap("<leader>hn", function()
			harpoon:list():select(3)
		end, "goto harpoon 3")
		KMap("<leader>hs", function()
			harpoon:list():select(4)
		end, "goto harpoon ")
	end,
}
