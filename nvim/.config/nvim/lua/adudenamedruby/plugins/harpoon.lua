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
        end, "add to h")
        KMap("<leader>hat", function()
            harpoon:list():replace_at(2)
        end, "add to t")
        KMap("<leader>han", function()
            harpoon:list():replace_at(3)
        end, "add to n")
        KMap("<leader>has", function()
            harpoon:list():replace_at(4)
        end, "add to s")

        KMap("<leader>hr", "", "remove")
        KMap("<leader>hrr", function()
            harpoon:list():remove()
        end, "remove current file")
        KMap("<leader>hrc", function()
            harpoon:list():clear()
        end, "clear harpoon")
        KMap("<leader>hrh", function()
            harpoon:list():remove_at(1)
        end, "clear h")
        KMap("<leader>hrt", function()
            harpoon:list():remove_at(2)
        end, "clear t")
        KMap("<leader>hrn", function()
            harpoon:list():remove_at(3)
        end, "clear n")
        KMap("<leader>hrs", function()
            harpoon:list():remove_at(4)
        end, "clear s")

        KMap("<leader>hv", function()
            -- toggle_telescope(harpoon:list())
            harpoon.ui:toggle_quick_menu(harpoon:list())
        end, "open harpoon list")

        KMap("<leader>hh", function()
            harpoon:list():select(1)
        end, "go to h")
        KMap("<leader>ht", function()
            harpoon:list():select(2)
        end, "go to t")
        KMap("<leader>hn", function()
            harpoon:list():select(3)
        end, "go to n")
        KMap("<leader>hs", function()
            harpoon:list():select(4)
        end, "go to s")
    end,
}
