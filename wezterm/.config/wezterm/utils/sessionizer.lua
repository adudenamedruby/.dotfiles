local wezterm = require("wezterm")
local act = wezterm.action

local M = {}

local fd = "/opt/homebrew/bin/fd"

M.toggle = function(window, pane)
	local projects = {}

	-- Search for Git Repos in ~/Developer
	local success_dev, stdout_dev, stderr_dev = wezterm.run_child_process({
		fd,
		"-HI",
		"^.git$",
		"--max-depth=4",
		"--prune",
		os.getenv("HOME") .. "/Developer",
	})

	if success_dev then
		for line in stdout_dev:gmatch("([^\n]*)\n?") do
			if line ~= "" then
				-- Strip the /.git at the end to get the project root
				local project = line:gsub("/.git.*$", "")
				local label = project
				local id = project:gsub(".*/", "")
				table.insert(projects, { label = tostring(label), id = tostring(id) })
			end
		end
	else
		wezterm.log_error("Failed to run fd on Developer: " .. stderr_dev)
	end

	-- Search for Subfolders in ~/.dotfiles
	local success_dot, stdout_dot, stderr_dot = wezterm.run_child_process({
		fd,
		"--color=never",
		"-H",
		"-E",
		".git",
		"-E",
		".meta",
		"-E",
		".jj",
		"-t",
		"d",
		"--max-depth=3",
		".",
		os.getenv("HOME") .. "/.dotfiles",
	})

	if success_dot and stdout_dot then
		local dot_paths = {}
		for line in stdout_dot:gmatch("([^\r\n]+)") do
			local trimmed = line:match("^%s*(.-)%s*$")
			if trimmed ~= nil and trimmed ~= "" then
				-- normalize: remove trailing slash and leading "./"
				local normalized = trimmed:gsub("/$", ""):gsub("^%./", "")
				table.insert(dot_paths, normalized)
			end
		end

		-- Sort longest-first so children come before parents (use length, not lexicographic)
		table.sort(dot_paths, function(a, b)
			return #a > #b
		end)

		local kept_paths = {}
		for _, path in ipairs(dot_paths) do
			local is_parent = false
			for _, child in ipairs(kept_paths) do
				-- child starts with "path/" -> path is a parent, so skip path
				if child:sub(1, #path + 1) == path .. "/" then
					is_parent = true
					break
				end
			end

			if not is_parent then
				table.insert(kept_paths, path)
				local id = path:match("[^/]+$") or path -- leaf folder name
				table.insert(projects, { label = path, id = id })
			end
		end
	else
		wezterm.log_error("Failed to run fd on .dotfiles: " .. tostring(stderr_dot))
	end

	table.sort(projects, function(a, b)
		return a.label < b.label
	end)

	-- Display Selector
	window:perform_action(
		act.InputSelector({
			action = wezterm.action_callback(function(win, _, id, label)
				if not id and not label then
					wezterm.log_info("Cancelled")
				else
					wezterm.log_info("Selected " .. label)
					win:perform_action(act.SwitchToWorkspace({ name = id, spawn = { cwd = label } }), pane)
				end
			end),
			fuzzy = true,
			title = "Select project",
			choices = projects,
		}),
		pane
	)
end

return M
