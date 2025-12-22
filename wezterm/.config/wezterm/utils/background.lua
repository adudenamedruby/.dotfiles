local wezterm = require("wezterm")
local M = {}

-- List of your favorite themes
local THEMES = {
	"Gruvbox Material (Gogh)",
	"Apple System Colors",
	"Tomorrow Night Burns",
	"Bitmute (terminal.sexy)",
	"Sequoia Monochrome",
	"Ocean Dark (Gogh)",
	"Trim Yer Beard (terminal.sexy)",
	"Bleh-1 (terminal.sexy)",
	"Catppuccin Mocha",
	"Afterglow",
	"Ef-Night",
	"Ayu Dark (Gogh)",
	"Pretty and Pastel (terminal.sexy)",
	"Everblush",
	"Darktooth (base16)",
	"N0tch2k",
	"PaperColor Dark (base16)",
	"FrontEndDelight",
	"Poimandres",
	"Spacedust",
	"Rosé Pine (base16)",
	"Hybrid (Gogh)",
	"Adventure",
	"Shaman",
	"Cloud (terminal.sexy)",
	"Wombat",
	"Glacier",
	"Operator Mono Dark",
	"Kasugano (terminal.sexy)",
	"Atelier Dune (base16)",
	"Mirage",
	"Birds Of Paradise (Gogh)",
	"Ocean (base16)",
	"Tomorrow Night (Gogh)",
	"Grayscale Dark (base16)",
	"Soft Server (Gogh)",
	"Fahrenheit",
	"Batman",
	"JWR dark (terminal.sexy)",
	"Embers (base16)",
	"Count Von Count (terminal.sexy)",
	"GruvboxDarkHard",
	"Cai (Gogh)",
	"Red Phoenix (terminal.sexy)",
	"Apathy (base16)",
	"Smyck (Gogh)",
	"Default (dark) (terminal.sexy)",
	"Arthur",
	"Twilight",
	"Apprentice (base16)",
	"Navy and Ivory (terminal.sexy)",
	"Shic (terminal.sexy)",
	"Pulp (terminal.sexy)",
	"Elemental",
	"Woodland (base16)",
	"DjangoRebornAgain",
	"Rosé Pine (Gogh)",
	"ICOrangePPL (Gogh)",
	"Breath (Gogh)",
	"Ashes (base16)",
	"BlulocoDark",
	"Green Screen (base16)",
	"Tomorrow (dark) (terminal.sexy)",
	"Mashup Colors (terminal.sexy)",
	"Aardvark Blue",
	"Arcoiris",
	"Aci (Gogh)",
	"Matrix",
	"Chiapre",
	"Pnevma",
	"3024 Night",
	"Wryan",
	"Atelierdune (dark) (terminal.sexy)",
	"Grape (Gogh)",
}

-- Get the day of the year (1-365/366)
local function get_day_of_year()
	return tonumber(os.date("%j"))
end

-- Get the daily rotating theme
M.get_daily_theme = function()
	local day = get_day_of_year()
	local index = ((day - 1) % #THEMES) + 1
	return THEMES[index]
end

return M
