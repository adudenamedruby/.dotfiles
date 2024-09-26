return {
	"tamton-aquib/duck.nvim",
	config = function()
		KMap("<leader>uD", "", "Duck")
		KMap("<leader>uDd", function()
			require("duck").hatch()
		end, "summon duck")
		KMap("<leader>uDc", function()
			require("duck").cook()
		end, "cook duck")
		KMap("<leader>uDa", function()
			require("duck").cook_all()
		end, "cook all ducks")
	end,
}
