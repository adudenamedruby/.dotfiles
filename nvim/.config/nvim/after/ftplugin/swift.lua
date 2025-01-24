vim.cmd([[ syntax match lambda "func" conceal cchar=λ ]])

local xcodebuild = require("xcodebuild.integrations.dap")

-- TODO: make sure to set path to your codelldb
-- local codelldbPath = os.getenv("HOME") .. "/Developer/codelldb-aarch64-darwin/extension/adapter/codelldb"
local mason_registry = require("mason-registry")
local codelldb = mason_registry.get_package("codelldb")
local codelldbPath = codelldb:get_install_path() .. "/extension/adapter/codelldb"

xcodebuild.setup(codelldbPath)
vim.keymap.set("n", "<leader>cr", xcodebuild.build_and_debug, { desc = "build & debug" })
vim.keymap.set("n", "<leader>dd", xcodebuild.debug_without_build, { desc = "debug without building" })
vim.keymap.set("n", "<leader>dt", xcodebuild.debug_tests, { desc = "debug tests" })
vim.keymap.set("n", "<leader>dT", xcodebuild.debug_class_tests, { desc = "debug class tests" })
vim.keymap.set("n", "<leader>db", xcodebuild.toggle_breakpoint, { desc = "toggle breakpoint" })
vim.keymap.set("n", "<leader>dB", xcodebuild.toggle_message_breakpoint, { desc = "toggle message breakpoint" })
vim.keymap.set("n", "<leader>dx", function()
    xcodebuild.terminate_session()
    require("dap").listeners.after["event_terminated"]["me"]()
end, { desc = "terminate debugger" })

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
