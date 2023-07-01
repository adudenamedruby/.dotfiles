
function ColorMyPencils(color)
    local catppuccin = "catppuccin-mocha"
	color = color or catppuccin

    local status_ok, _ = pcall(vim.cmd, "colorscheme " .. color)
    if not status_ok then
        vim.notify("colorscheme " .. color .. " not found!")
        return
    end

	vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

ColorMyPencils()
