-- Pull in the wezterm API
local wezterm = require("wezterm")
local bar = wezterm.plugin.require("https://github.com/adriankarlen/bar.wezterm")
local sessionizer = require("utils.sessionizer")
local background = require("utils.background")
local smart_nav = require("utils.smart_nav")
local act = wezterm.action
-- local assets = wezterm.config_dir .. "/assets"

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- For example, changing the color scheme:
config.color_scheme = background.get_default_theme()

config.colors = {
	-- Override the cursor colors while preserving the rest of the theme
	cursor_bg = "#e04ba4", -- Set the cursor background color to red
	cursor_fg = "#000000", -- Set the cursor text color to white (if cursor is over text)
	cursor_border = "#e04ba4", -- Set the cursor border color to red
}

local fancy = false
if fancy then
	config.background = {
		background.get_background(),
	}
end

config.window_background_opacity = 0.9
config.macos_window_background_blur = 30
-- config.window_background_opacity = 1

config.font = wezterm.font("FiraCode Nerd Font Mono")
config.font_size = 16
config.adjust_window_size_when_changing_font_size = false

config.window_decorations = "RESIZE"
-- config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }
config.scrollback_lines = 5000

-- config.enable_mouse_reporting = true
-- config.automatically_select_copy = true

-- tab bar
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.tab_and_split_indices_are_zero_based = false

-- edit mode
local function open_scrollback_in_vim(window, pane, nlines)
	nlines = nlines or 2000

	local ok, text = pcall(function()
		return pane:get_lines_as_text(nlines)
	end)
	if not ok or not text then
		window:toast_notification("WezTerm", "Failed to read pane scrollback", nil, 4000)
		return
	end

	local timestamp = tostring(os.time())
	local filename = "/tmp/wezterm-scrollback-" .. timestamp .. ".txt"

	local f, ferr = io.open(filename, "w")
	if not f then
		window:toast_notification("WezTerm", "Failed to open file: " .. (ferr or "unknown"), nil, 4000)
		return
	end
	f:write(text)
	f:close()

	local editor = os.getenv("EDITOR") or "nvim"
	local cmd = { "sh", "-lc", string.format("%s +$ %q", editor, filename) }

	window:perform_action(act({ SpawnCommandInNewTab = { args = cmd } }), pane)
end

-- keys
config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = {
	-- zooming
	{ key = "=", mods = "CTRL", action = "DisableDefaultAssignment" },
	{ key = "-", mods = "CTRL", action = "DisableDefaultAssignment" },
	{ mods = "CMD", key = "=", action = act.IncreaseFontSize },
	{ mods = "CMD", key = "-", action = act.DecreaseFontSize },
	{ mods = "CMD", key = "0", action = act.ResetFontSize },
	-- wezterm commands
	{ mods = "LEADER", key = "R", action = act.ReloadConfiguration },
	{ mods = "LEADER", key = "D", action = wezterm.action.ShowDebugOverlay },
	-- control commands
	{ mods = "LEADER", key = "t", action = act.SpawnTab("CurrentPaneDomain") },
	{ mods = "LEADER", key = "d", action = act.CloseCurrentPane({ confirm = false }) },
	{ mods = "LEADER", key = "p", action = act.ActivateTabRelative(-1) },
	{ mods = "LEADER", key = "n", action = act.ActivateTabRelative(1) },
	{ mods = "LEADER", key = "w", action = act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }) },
	{ mods = "LEADER", key = "v", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ mods = "LEADER", key = "s", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ mods = "LEADER", key = "g", action = wezterm.action_callback(sessionizer.toggle) },
	{ mods = "LEADER", key = "c", action = act.ActivateCopyMode },
	{
		key = "r",
		mods = "LEADER",
		action = wezterm.action_callback(function(window, pane)
			open_scrollback_in_vim(window, pane, 2000)
		end),
	},
	-- Smart navigation with CTRL+hjkl (works with nvim splits)
	smart_nav.split_nav("move", "CTRL", "h", "Left"),
	smart_nav.split_nav("move", "CTRL", "j", "Down"),
	smart_nav.split_nav("move", "CTRL", "k", "Up"),
	smart_nav.split_nav("move", "CTRL", "l", "Right"),
	-- Smart scrolling with CTRL+u/d (works with nvim)
	smart_nav.smart_scroll("CTRL", "u", -0.5),
	smart_nav.smart_scroll("CTRL", "d", 0.5),
	-- {
	-- 	mods = "LEADER",
	-- 	key = "r",
	-- 	action = act.PromptInputLine({
	-- 		description = "Rename tab: ",
	-- 		action = wezterm.action_callback(function(window, _, line)
	-- 			if line then
	-- 				window:active_tab():set_title(line)
	-- 			end
	-- 		end),
	-- 	}),
	-- },
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "h",
	-- 		action = act.ActivatePaneDirection("Left"),
	-- 	},
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "j",
	-- 		action = act.ActivatePaneDirection("Down"),
	-- 	},
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "k",
	-- 		action = act.ActivatePaneDirection("Up"),
	-- 	},
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "l",
	-- 		action = act.ActivatePaneDirection("Right"),
	-- 	},
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "LeftArrow",
	-- 		action = act.AdjustPaneSize({ "Left", 5 }),
	-- 	},
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "RightArrow",
	-- 		action = act.AdjustPaneSize({ "Right", 5 }),
	-- 	},
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "DownArrow",
	-- 		action = act.AdjustPaneSize({ "Down", 5 }),
	-- 	},
	-- 	{
	-- 		mods = "LEADER",
	-- 		key = "UpArrow",
	-- 		action = act.AdjustPaneSize({ "Up", 5 }),
	-- 	},
	-- 	-- {
	-- 	-- 	mods = "LEADER",
	-- 	-- 	key = "G",
	-- 	-- 	action = act.callback(function(window, pane)
	-- 	-- 		local cwd = pane:get_current_working_dir()
	-- 	-- 		if cwd then
	-- 	-- 			window:perform_action(
	-- 	-- 				act.SpawnCommandInNewWindow({
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
}
--
-- for i = 1, 9 do
-- 	-- leader + number to activate that tab
-- 	table.insert(config.keys, {
-- 		key = tostring(i),
-- 		mods = "LEADER",
-- 		action = act.ActivateTab(i - 1),
-- 	})
-- end
--

-- Use if not using the bar plugin
-- leader command status
-- wezterm.on("update-right-status", function(window, _)
-- 	local SOLID_LEFT_ARROW = ""
-- 	local ARROW_FOREGROUND = { Foreground = { Color = "#c6a0f6" } }
-- 	local BACKGROUND = { Background = { Color = "#000000" } }
-- 	local workspace = window:active_workspace()
-- 	local prefix = "  [" .. workspace .. "]  "
--
-- 	if window:leader_is_active() then
-- 		prefix = " " .. utf8.char(0x1f30a) .. prefix
-- 		SOLID_LEFT_ARROW = utf8.char(0xe0b2)
-- 		BACKGROUND = { Background = { Color = "#8E2A2D" } }
-- 	end
--
-- 	if window:active_tab():tab_id() ~= 0 then
-- 		ARROW_FOREGROUND = { Foreground = { Color = "#1e2030" } }
-- 	end -- arrow color based on if tab is first pane
--
-- 	window:set_left_status(wezterm.format({
-- 		BACKGROUND,
-- 		{ Text = prefix },
-- 		ARROW_FOREGROUND,
-- 		{ Text = SOLID_LEFT_ARROW },
-- 	}))
--
-- 	-- window:set_right_status(wezterm.format({
-- 	-- 	{ Text = "  [" .. workspace .. "]  " },
-- 	-- }))
-- end)

bar.apply_to_config(config, {
	modules = {
		workspaces = {
			color = 1,
		},
		leader = {
			icon = wezterm.nerdfonts.oct_rocket,
			color = 3,
		},
		pane = {
			enabled = false,
		},
	},
})

-- and finally, return the configuration to wezterm
return config
