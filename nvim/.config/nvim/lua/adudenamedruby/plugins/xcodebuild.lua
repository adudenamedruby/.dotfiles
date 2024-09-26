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

    -- stylua: ignore start
    -- code menu
    KMap("<leader>cb", "<cmd>XcodebuildBuild<cr>", "build project")
    KMap("<leader>cB", "<cmd>XcodebuildBuildForTesting<cr>", "build for testing")
    -- ("<leader>cr", "<cmd>XcodebuildBuildRun<cr>", "build & run project")
    KMap("<leader>c.", "<cmd>XcodebuildCancel<cr>", "cancel run action")
    KMap("<leader>ca", "<cmd>XcodebuildCodeActions<cr>", "show code actions")
    KMap("<leader>cq", "<cmd>Telescope quickfix<cr>", "show quickFix list")
    KMap("<leader>cl", "<cmd>XcodebuildQuickfixLine<cr>", "quickfix line")

    -- utilities menu
    KMap("<leader>cu", "", "basic utilities")
    KMap("<leader>cup", "<cmd>XcodebuildCleanProject<cr>", "clean project")
    KMap("<leader>cud", "<cmd>XcodebuildCleanDerivedData<cr>", "clean derived data")
    KMap("<leader>cul", "<cmd>XcodebuildToggleLogs<cr>", "show Xcodebuild logs")

    -- project files menu
    KMap("<leader>cp", "", "project files")
    KMap("<leader>cpn", "<cmd>XcodebuildCreateNewFile<cr>", "new file")
    KMap("<leader>cpN", "<cmd>XcodebuildAddCurrentFile<cr>", "add current file to targets")
    KMap("<leader>cpr", "<cmd>XcodebuildRenameCurrentFile<cr>", "rename current file")
    KMap("<leader>cpd", "<cmd>XcodebuildDeleteCurrentFile<cr>", "delete current file")
    KMap("<leader>cpD", "<cmd>XcodebuildDeleteCurrentGroup<cr>", "delete current group")
    KMap("<leader>cpg", "<cmd>XcodebuildCreateNewGroup<cr>", "create new group")
    KMap("<leader>cpG", "<cmd>XcodebuildAddCurrentGroup<cr>", "add current group to targets")
    KMap("<leader>cps", "<cmd>XcodebuildShowCurrentFileTargets<cr>", "show current file targets")
    KMap("<leader>cpu", "<cmd>XcodebuildUpdateCurrentFileTargets<cr>", "update current file targets")

    -- test menu
    KMap("<leader>ct", "", "tests")
    KMap("<leader>cta", "<cmd>XcodebuildTest<cr>", "run all tests")
    KMap("<leader>ctt", "<cmd>XcodebuildTestSelected<cr>", "run selected tests", "v")
    KMap("<leader>ctc", "<cmd>XcodebuildTestClass<cr>", "run this test class")
    KMap("<leader>ctf", "<cmd>XcodebuildTestFailing<cr>", "rerun failed test")
    KMap("<leader>ctr", "<cmd>XcodebuildTestRepeat<cr>", "repeat last test")
    KMap("<leader>ctn", "<cmd>XcodebuildTestNearest<cr>", "test nearest")
    KMap("<leader>ctp", "<cmd>XcodebuildSelectTestPlan<cr>", "select test plan")

    -- xcode menu
    KMap("<leader>cx", "", "Xcode actions")
    KMap("<leader>cxb", "<cmd>XcodebuildPicker<cr>", "show Xcodebuild actions")
    KMap("<leader>cxp", "<cmd>XcodebuildProjectManager<cr>", "show project manager actions")
    KMap("<leader>cxc", "<cmd>XcodebuildToggleCodeCoverage<cr>", "show code coverage")
    KMap("<leader>cxC", "<cmd>XcodebuildShowCodeCoverageReport<cr>", "show code coverage report")
    KMap("<leader>cxe", "<cmd>XcodebuildTestExplorerToggle<cr>", "toggle test explorer")
    KMap("<leader>cxs", "<cmd>XcodebuildSelectScheme<cr>", "select scheme")
    KMap("<leader>cxt", "<cmd>XcodebuildSelectTestPlan<cr>", "select test plan")
    KMap("<leader>cxd", "<cmd>XcodebuildSelectDevice<cr>", "select device")
    KMap("<leader>cx.", "<cmd>XcodebuildBootSimulator<cr>", "boot selected simulator")

    -- xcode project manager menu
    KMap("<leader>cxp", "<cmd>XcodebuildSelectTestPlan<cr>", "select test plan")
	end,
}
