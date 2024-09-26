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
    KMap("n", "<leader>cb", "<cmd>XcodebuildBuild<cr>", "build project")
    KMap("n", "<leader>cB", "<cmd>XcodebuildBuildForTesting<cr>", "build for testing")
    -- vim.keymap.set("n", "<leader>cr", "<cmd>XcodebuildBuildRun<cr>", { desc = "build & run project" })
    KMap("n", "<leader>c.", "<cmd>XcodebuildCancel<cr>", "cancel run action")
    KMap("n", "<leader>ca", "<cmd>XcodebuildCodeActions<cr>", "show code actions")
    KMap("n", "<leader>cq", "<cmd>Telescope quickfix<cr>", "show quickFix list")
    KMap("n", "<leader>cl", "<cmd>XcodebuildQuickfixLine<cr>", "quickfix line")

    -- utilities menu
    KMap("n", "<leader>cu", "", "basic utilities")
    KMap("n", "<leader>cup", "<cmd>XcodebuildCleanProject<cr>", "clean project")
    KMap("n", "<leader>cud", "<cmd>XcodebuildCleanDerivedData<cr>", "clean derived data")
    KMap("n", "<leader>cul", "<cmd>XcodebuildToggleLogs<cr>", "show Xcodebuild logs")

    -- project files menu
    KMap("n", "<leader>cp", "", "project files")
    KMap("n", "<leader>cpn", "<cmd>XcodebuildCreateNewFile<cr>", "new file")
    KMap("n", "<leader>cpN", "<cmd>XcodebuildAddCurrentFile<cr>", "add current file to targets")
    KMap("n", "<leader>cpr", "<cmd>XcodebuildRenameCurrentFile<cr>", "rename current file")
    KMap("n", "<leader>cpd", "<cmd>XcodebuildDeleteCurrentFile<cr>", "delete current file")
    KMap("n", "<leader>cpD", "<cmd>XcodebuildDeleteCurrentGroup<cr>", "delete current group")
    KMap("n", "<leader>cpg", "<cmd>XcodebuildCreateNewGroup<cr>", "create new group")
    KMap("n", "<leader>cpG", "<cmd>XcodebuildAddCurrentGroup<cr>", "add current group to targets")
    KMap("n", "<leader>cps", "<cmd>XcodebuildShowCurrentFileTargets<cr>", "show current file targets")
    KMap("n", "<leader>cpu", "<cmd>XcodebuildUpdateCurrentFileTargets<cr>", "update current file targets")

    -- test menu
    KMap("n", "<leader>ct", "", "tests")
    KMap("n", "<leader>cta", "<cmd>XcodebuildTest<cr>", "run all tests")
    KMap("v", "<leader>ctt", "<cmd>XcodebuildTestSelected<cr>", "run selected tests")
    KMap("n", "<leader>ctc", "<cmd>XcodebuildTestClass<cr>", "run this test class")
    KMap("n", "<leader>ctf", "<cmd>XcodebuildTestFailing<cr>", "rerun failed test")
    KMap("n", "<leader>ctr", "<cmd>XcodebuildTestRepeat<cr>", "repeat last test")
    KMap("n", "<leader>ctn", "<cmd>XcodebuildTestNearest<cr>", "test nearest")
    KMap("n", "<leader>ctp", "<cmd>XcodebuildSelectTestPlan<cr>", "select test plan")

    -- xcode menu
    KMap("n", "<leader>cx", "", "Xcode actions")
    KMap("n", "<leader>cxb", "<cmd>XcodebuildPicker<cr>", "show Xcodebuild actions")
    KMap("n", "<leader>cxp", "<cmd>XcodebuildProjectManager<cr>", "show project manager actions")
    KMap("n", "<leader>cxc", "<cmd>XcodebuildToggleCodeCoverage<cr>", "show code coverage")
    KMap("n", "<leader>cxC", "<cmd>XcodebuildShowCodeCoverageReport<cr>", "show code coverage report")
    KMap("n", "<leader>cxe", "<cmd>XcodebuildTestExplorerToggle<cr>", "toggle test explorer")
    KMap("n", "<leader>cxs", "<cmd>XcodebuildSelectScheme<cr>", "select scheme")
    KMap("n", "<leader>cxt", "<cmd>XcodebuildSelectTestPlan<cr>", "select test plan")
    KMap("n", "<leader>cxd", "<cmd>XcodebuildSelectDevice<cr>", "select device")
    KMap("n", "<leader>cx.", "<cmd>XcodebuildBootSimulator<cr>", "boot selected simulator")

    -- xcode project manager menu
    KMap("n", "<leader>cxp", "<cmd>XcodebuildSelectTestPlan<cr>", "select test plan")
	end,
}
