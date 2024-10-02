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

config.colors = {
	-- Override the cursor colors while preserving the rest of the theme
	cursor_bg = "#e04ba4", -- Set the cursor background color to red
	cursor_fg = "#000000", -- Set the cursor text color to white (if cursor is over text)
	cursor_border = "#e04ba4", -- Set the cursor border color to red
}

local fancy = true
if fancy then
	config.background = {
		-- b.get_background(),
		b.get_random_animation(assets),
	}
end

config.window_background_opacity = 0.995
config.window_background_opacity = 1

config.font = wezterm.font("FiraCode Nerd Font Mono")
config.font_size = 16
config.adjust_window_size_when_changing_font_size = false

config.window_decorations = "RESIZE"

-- config.enable_mouse_reporting = true
-- config.automatically_select_copy = true

-- tab bar
config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.tab_and_split_indices_are_zero_based = false

-- tmux
-- config.leader = { key = "w", mods = "CTRL", timeout_milliseconds = 2000 }
-- config.keys = {
-- 	{
-- 		mods = "LEADER",
-- 		key = "c",
-- 		action = wezterm.action.SpawnTab("CurrentPaneDomain"),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "d",
-- 		action = wezterm.action.CloseCurrentPane({ confirm = true }),
-- 	},
-- 	{
-- 		mods = "LEADER|CTRL",
-- 		key = "h",
-- 		action = wezterm.action.ActivateTabRelative(-1),
-- 	},
-- 	{
-- 		mods = "LEADER|CTRL",
-- 		key = "l",
-- 		action = wezterm.action.ActivateTabRelative(1),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "v",
-- 		action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "s",
-- 		action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "h",
-- 		action = wezterm.action.ActivatePaneDirection("Left"),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "j",
-- 		action = wezterm.action.ActivatePaneDirection("Down"),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "k",
-- 		action = wezterm.action.ActivatePaneDirection("Up"),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "l",
-- 		action = wezterm.action.ActivatePaneDirection("Right"),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "LeftArrow",
-- 		action = wezterm.action.AdjustPaneSize({ "Left", 5 }),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "RightArrow",
-- 		action = wezterm.action.AdjustPaneSize({ "Right", 5 }),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "DownArrow",
-- 		action = wezterm.action.AdjustPaneSize({ "Down", 5 }),
-- 	},
-- 	{
-- 		mods = "LEADER",
-- 		key = "UpArrow",
-- 		action = wezterm.action.AdjustPaneSize({ "Up", 5 }),
-- 	},
-- 	-- {
-- 	-- 	mods = "LEADER",
-- 	-- 	key = "g",
-- 	-- 	action = wezterm.action_callback(function(window, pane)
-- 	-- 		local cwd = pane:get_current_working_dir()
-- 	-- 		if cwd then
-- 	-- 			window:perform_action(
-- 	-- 				wezterm.action.SpawnCommandInNewWindow({
-- 	-- 					args = { "lazygit" },
-- 	-- 					cwd = cwd,
-- 	-- 					set_environment_variables = {
-- 	-- 						PATH = os.getenv("PATH"),
-- 	-- 					},
-- 	-- 				}),
-- 	-- 				pane
-- 	-- 			)
-- 	-- 		end
-- 	-- 	end),
-- 	-- },
-- 	{
-- 		mods = "LEADER",
-- 		key = "r",
-- 		action = wezterm.action.PromptInputLine({
-- 			description = "Rename tab: ",
-- 			action = wezterm.action_callback(function(window, _, line)
-- 				if line then
-- 					window:active_tab():set_title(line)
-- 				end
-- 			end),
-- 		}),
-- 	},
-- }
--
-- for i = 1, 9 do
-- 	-- leader + number to activate that tab
-- 	table.insert(config.keys, {
-- 		key = tostring(i),
-- 		mods = "LEADER",
-- 		action = wezterm.action.ActivateTab(i - 1),
-- 	})
-- end
--
-- leader command status
wezterm.on("update-right-status", function(window, _)
	local SOLID_LEFT_ARROW = ""
	local ARROW_FOREGROUND = { Foreground = { Color = "#c6a0f6" } }
	local prefix = ""

	if window:leader_is_active() then
		prefix = " " .. utf8.char(0x1f30a) -- ocean wave
		SOLID_LEFT_ARROW = utf8.char(0xe0b2)
	end

	if window:active_tab():tab_id() ~= 0 then
		ARROW_FOREGROUND = { Foreground = { Color = "#1e2030" } }
	end -- arrow color based on if tab is first pane

	window:set_left_status(wezterm.format({
		{ Background = { Color = "#b7bdf8" } },
		{ Text = prefix },
		ARROW_FOREGROUND,
		{ Text = SOLID_LEFT_ARROW },
	}))
end)

-- and finally, return the configuration to wezterm
return config
