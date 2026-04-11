local function setupListeners()
    local U = require("adudenamedruby.core.utils")
    local dap = require("dap")
    local areSet = false

    dap.listeners.after["event_initialized"]["me"] = function()
        if not areSet then
            areSet = true
            U.GLMap("ddc", dap.continue, "continue")
            U.GLMap("ddC", dap.run_to_cursor, "run to cursor")
            U.GLMap("dds", dap.step_over, "step over")
            U.GLMap("ddi", dap.step_into, "step into")
            U.GLMap("ddo", dap.step_out, "step out")
            U.GLMap("ddh", require("dap.ui.widgets").hover, "hover", { "n", "v" })
            U.GLMap("dde", require("dapui").eval, "eval", { "n", "v" })
        end
    end

    dap.listeners.after["event_terminated"]["me"] = function()
        if areSet then
            areSet = false
            vim.keymap.del("n", "<leader>ddc")
            vim.keymap.del("n", "<leader>ddC")
            vim.keymap.del("n", "<leader>dds")
            vim.keymap.del("n", "<leader>ddi")
            vim.keymap.del("n", "<leader>ddo")
            vim.keymap.del({ "n", "v" }, "<leader>ddh")
            vim.keymap.del({ "n", "v" }, "<leader>dde")
        end
    end
end

return {
    "mfussenegger/nvim-dap",
    dependencies = {},
    config = function()
        local define = vim.fn.sign_define
        define("DapBreakpoint", { text = "", texthl = "DiagnosticError", linehl = "", numhl = "" })
        define("DapBreakpointRejected", { text = "", texthl = "DiagnosticError", linehl = "", numhl = "" })
        define("DapStopped", { text = "", texthl = "DiagnosticOk", linehl = "", numhl = "" })
        define("DapLogPoint", { text = "", texthl = "DiagnosticInfo", linehl = "", numhl = "" })
        define("DapLogPoint", { text = "", texthl = "DiagnosticInfo", linehl = "", numhl = "" })

        local U = require("adudenamedruby.core.utils")
        U.GLMap("ddb", ":DapToggleBreakpoint<CR>", "toggle breakpoint")
        U.GLMap("ddB", ":DapClearBreakpoints<CR>", "clear all breakpoints")

        setupListeners()

        --when breakpoint is hit, it sets the focus to the buffer with the breakpoint
        require("dap").defaults.fallback.switchbuf = "usetab,uselast"
    end,
}
