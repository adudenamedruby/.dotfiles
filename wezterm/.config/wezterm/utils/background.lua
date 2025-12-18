local h = require("utils.helpers")
local wezterm = require("wezterm")
local M = {}

-- List of your favorite themes
local THEMES = {
	"Catppuccin Mocha",
	"rose-pine",
	"Glacier",
	"matrix",
	"GruvboxDarkHard",
	"MonaLisa",
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
	file:write(string.format('  day = %d,\n', cache.day))
	file:write('  windows = {\n')
	for window_id, theme in pairs(cache.windows) do
		file:write(string.format('    [%d] = "%s",\n', window_id, theme))
	end
	file:write('  }\n')
	file:write('}\n')
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
			windows = {}
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
