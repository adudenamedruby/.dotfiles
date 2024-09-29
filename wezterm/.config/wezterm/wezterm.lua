-- Pull in the wezterm API
local wezterm = require("wezterm")

local b = require("utils.background")
local assets = wezterm.config_dir .. "/assets"

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- For example, changing the color scheme:
config.color_scheme = b.get_default_theme()

local fancy = true
if fancy then
	config.background = {
		-- b.get_background(),
		b.get_random_animation(assets),
	}
end

config.window_background_opacity = 0.995

config.font = wezterm.font("FiraCode Nerd Font Mono")
config.font_size = 16
config.adjust_window_size_when_changing_font_size = false

config.window_decorations = "RESIZE"

-- tab bar
config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.tab_and_split_indices_are_zero_based = true

-- and finally, return the configuration to wezterm
return config
