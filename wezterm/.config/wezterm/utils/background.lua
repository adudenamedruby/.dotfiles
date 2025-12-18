local h = require("utils.helpers")
local wezterm = require("wezterm")
local M = {}

-- List of your favorite themes
local THEMES = {
	"Gruvbox Material (Gogh)",
	"Apple System Colors",
	"Tomorrow Night Burns",
	"Bitmute (terminal.sexy)",
	"Sequoia Monochrome",
	"Arthur",
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
	"Pnevma",
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

-- Cache file to store window-theme mappings
local CACHE_FILE = os.getenv("HOME") .. "/.config/wezterm/.theme_cache.lua"

-- Get the day of the year (1-365/366)
local function get_day_of_year()
	return tonumber(os.date("%j"))
end

-- Get the theme for the first window based on the current day
local function get_daily_theme()
	local day = get_day_of_year()
	local index = ((day - 1) % #THEMES) + 1
	return THEMES[index]
end

-- Load the theme cache from disk
local function load_cache()
	local file = io.open(CACHE_FILE, "r")
	if not file then
		return { day = 0, windows = {} }
	end

	local content = file:read("*all")
	file:close()

	-- Safely load the cache
	local chunk, err = load("return " .. content)
	if not chunk then
		return { day = 0, windows = {} }
	end

	local success, cache = pcall(chunk)
	if not success then
		return { day = 0, windows = {} }
	end

	return cache
end

-- Save the theme cache to disk
local function save_cache(cache)
	local file = io.open(CACHE_FILE, "w")
	if not file then
		return
	end

	file:write("{\n")
	file:write(string.format("  day = %d,\n", cache.day))
	file:write("  windows = {\n")
	for window_id, theme in pairs(cache.windows) do
		file:write(string.format('    [%d] = "%s",\n', window_id, theme))
	end
	file:write("  }\n")
	file:write("}\n")
	file:close()
end

-- Get a random theme excluding the given theme
local function get_random_theme_excluding(excluded_theme)
	local available_themes = {}
	for _, theme in ipairs(THEMES) do
		if theme ~= excluded_theme then
			table.insert(available_themes, theme)
		end
	end

	if #available_themes == 0 then
		return excluded_theme -- Fallback if only one theme exists
	end

	return available_themes[math.random(#available_themes)]
end

-- Get theme for a specific window
M.get_theme_for_window = function(window)
	local window_id = window:window_id()
	local current_day = get_day_of_year()

	-- Load cache
	local cache = load_cache()

	-- Reset cache if it's a new day
	if cache.day ~= current_day then
		cache = {
			day = current_day,
			windows = {},
		}
	end

	-- Check if this window already has a theme assigned
	if cache.windows[window_id] then
		return cache.windows[window_id]
	end

	-- Determine if this is the first window of the day
	local is_first_window = next(cache.windows) == nil

	local theme
	if is_first_window then
		-- First window gets the daily rotating theme
		theme = get_daily_theme()
	else
		-- Other windows get a random theme (excluding the daily theme)
		local daily_theme = get_daily_theme()
		theme = get_random_theme_excluding(daily_theme)
	end

	-- Save the assignment
	cache.windows[window_id] = theme
	save_cache(cache)

	return theme
end

-- Set theme manually for a specific window
M.set_theme_for_window = function(window, theme_name)
	local window_id = window:window_id()
	local current_day = get_day_of_year()

	-- Load cache
	local cache = load_cache()

	-- Initialize cache if needed
	if cache.day ~= current_day then
		cache = {
			day = current_day,
			windows = {},
		}
	end

	-- Save the manual override
	cache.windows[window_id] = theme_name
	save_cache(cache)

	-- Apply the theme immediately
	local overrides = window:get_config_overrides() or {}
	overrides.color_scheme = theme_name
	window:set_config_overrides(overrides)
end

-- Legacy function for backward compatibility (uses random theme)
M.get_default_theme = function()
	return h.get_random_entry(THEMES)
end

M.get_background = function(dark, light)
	dark = dark or 0.85
	light = light or 0.9

	return {
		source = {
			Gradient = {
				colors = { h.is_dark and "#000000" or "#ffffff" },
			},
		},
		width = "100%",
		height = "100%",
		opacity = h.is_dark and dark or light,
	}
end

M.get_wallpaper = function(wallpaper)
	return {
		source = { File = { path = wallpaper } },
		height = "Cover",
		width = "Cover",
		horizontal_align = "Center",
		repeat_x = "NoRepeat",
		repeat_y = "NoRepeat",
		opacity = 1,
	}
end

M.get_random_wallpaper = function(dir)
	dir = dir or os.getenv("HOME") .. "/.config/wezterm/assets/***.{jpg,jpeg,png}"
	local wallpapers = {}
	for _, v in ipairs(wezterm.glob(dir)) do
		if not string.match(v, "%.DS_Store$") then
			table.insert(wallpapers, v)
		end
	end

	local wallpaper = h.get_random_entry(wallpapers)

	return M.get_wallpaper(wallpaper)
end

M.get_animation = function(animation)
	return {
		source = {
			File = {
				path = animation,
				speed = 0.001,
			},
		},
		repeat_x = "NoRepeat",
		repeat_y = "NoRepeat",
		vertical_align = "Middle",
		width = "100%",
		height = "Cover",
		opacity = 0.45,
		hsb = {
			hue = 0.9,
			saturation = 0.8,
			brightness = 0.1,
		},
	}
end

M.get_random_animation = function(dir)
	dir = dir or os.getenv("HOME") .. "/.config/wezterm/assets"
	local animations = {}
	for _, v in ipairs(wezterm.glob(dir)) do
		if not string.match(v, "%.DS_Store$") then
			table.insert(animations, v)
		end
	end

	local animation = h.get_random_entry(animations)

	return M.get_animation(animation)
end

return M
