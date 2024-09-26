local function setupListeners()
	local dap = require("dap")
	local areSet = false

	dap.listeners.after["event_initialized"]["me"] = function()
		if not areSet then
			areSet = true
			KMap("<leader>dc", dap.continue, "continue")
			KMap("<leader>dC", dap.run_to_cursor, "run to cursor")
			KMap("<leader>ds", dap.step_over, "step over")
			KMap("<leader>di", dap.step_into, "step into")
			KMap("<leader>do", dap.step_out, "step out")
			KMap("<leader>dh", require("dap.ui.widgets").hover, "hover", { "n", "v" })
			KMap("<leader>de", require("dapui").eval, "eval", { "n", "v" })
		end
	end

	dap.listeners.after["event_terminated"]["me"] = function()
		if areSet then
			areSet = false
			vim.keymap.del("n", "<leader>dc")
			vim.keymap.del("n", "<leader>dC")
			vim.keymap.del("n", "<leader>ds")
			vim.keymap.del("n", "<leader>di")
			vim.keymap.del("n", "<leader>do")
			vim.keymap.del({ "n", "v" }, "<leader>dh")
			vim.keymap.del({ "n", "v" }, "<leader>de")
		end
	end
end

return {
	"mfussenegger/nvim-dap",
	dependencies = {
		"wojciech-kulik/xcodebuild.nvim",
	},
	config = function()
		local xcodebuild = require("xcodebuild.integrations.dap")

		-- TODO: make sure to set path to your codelldb
		local codelldbPath = os.getenv("HOME") .. "/Developer/codelldb-aarch64-darwin/extension/adapter/codelldb"
		xcodebuild.setup(codelldbPath)

		local define = vim.fn.sign_define
		define("DapBreakpoint", { text = "", texthl = "DiagnosticError", linehl = "", numhl = "" })
		define("DapBreakpointRejected", { text = "", texthl = "DiagnosticError", linehl = "", numhl = "" })
		define("DapStopped", { text = "", texthl = "DiagnosticOk", linehl = "", numhl = "" })
		define("DapLogPoint", { text = "", texthl = "DiagnosticInfo", linehl = "", numhl = "" })
		define("DapLogPoint", { text = "", texthl = "DiagnosticInfo", linehl = "", numhl = "" })

		setupListeners()

		--when breakpoint is hit, it sets the focus to the buffer with the breakpoint
		require("dap").defaults.fallback.switchbuf = "usetab,uselast"

    --stylua: ignore start
    vim.keymap.set("n", "<leader>cr", xcodebuild.build_and_debug, { desc = "build & debug" })
    vim.keymap.set("n", "<leader>dd", xcodebuild.debug_without_build, { desc = "debug without building" })
    vim.keymap.set("n", "<leader>dt", xcodebuild.debug_tests, { desc = "debug tests" })
    vim.keymap.set("n", "<leader>dT", xcodebuild.debug_class_tests, { desc = "debug class tests" })
    vim.keymap.set("n", "<leader>tb", xcodebuild.toggle_breakpoint, { desc = "toggle breakpoint" })
    vim.keymap.set("n", "<leader>tB", xcodebuild.toggle_message_breakpoint, { desc = "toggle message breakpoint" })
    vim.keymap.set("n", "<leader>dx", function()
        xcodebuild.terminate_session()
        require("dap").listeners.after["event_terminated"]["me"]()
    end, { desc = "terminate debugger" })
		--stylua: ignore end
	end,
}
