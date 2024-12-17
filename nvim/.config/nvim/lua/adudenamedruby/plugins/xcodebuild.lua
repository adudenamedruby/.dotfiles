local progress_handle

return {
    "wojciech-kulik/xcodebuild.nvim",
    dependencies = {
        "nvim-telescope/telescope.nvim",
        "nvim-tree/nvim-tree.lua",
        "MunifTanjim/nui.nvim",
    },

    config = function()
        require("xcodebuild").setup({
            show_build_progress_bar = false,
            prepare_snapshot_test_previews = false,
            logs = {
                auto_open_on_failed_tests = true,
                auto_open_on_failed_build = true,
                auto_focus = false,
                auto_close_on_app_launch = true,
                only_summary = true,
                notify = function(message, severity)
                    local fidget = require("fidget")
                    if progress_handle then
                        progress_handle.message = message
                        if not message:find("Loading") then
                            progress_handle:finish()
                            progress_handle = nil
                            if vim.trim(message) ~= "" then
                                fidget.notify(message, severity)
                            end
                        end
                    else
                        fidget.notify(message, severity)
                    end
                end,
                notify_progress = function(message)
                    local progress = require("fidget.progress")

                    if progress_handle then
                        progress_handle.title = ""
                        progress_handle.message = message
                    else
                        progress_handle = progress.handle.create({
                            message = message,
                            lsp_client = { name = "xcodebuild.nvim" },
                        })
                    end
                end,
            },
            code_coverage = {
                enabled = true,
            },
            integrations = {
                nvim_tree = {
                    enabled = true,
                    guess_target = true,
                    should_update_project = function(path)
                        return true
                    end,
                },
            },
        })
    end,
}
