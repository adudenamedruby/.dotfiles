local U = require("adudenamedruby.core.utils")

U.GLMap("cct", "<cmd>RustLsp testables<CR>", "testables")
U.GLMap("ccT", "<cmd>RustLsp test<CR>", "test")
U.GLMap("ccr", "<cmd>RustLsp runnables<CR>", "runnables")
U.GLMap("ccR", "<cmd>RustLsp run<CR>", "run")
U.GLMap("ccd", "<cmd>RustLsp debuggables<CR>", "debuggables")
U.GLMap("ccD", "<cmd>RustLsp debug<CR>", "debug")
U.GLMap("cd", "<cmd>RustLsp openDocs<CR>", "open docs at cursor")
U.GLMap("ce", "<cmd>RustLsp expandMacro<CR>", "expand macro")
U.GLMap("cH", "<cmd>RustLsp hover actions<CR>", "hover actions")
U.GLMap("de", "<cmd>RustLsp explainError<CR>", "explain error")
U.GLMap("dr", "<cmd>RustLsp renderDiagnostic<CR>", "render diagnostic")
U.GLMap("dR", "<cmd>RustLsp relatedDiagnostics<CR>", "related diagnostic")

vim.opt_local.colorcolumn = "100"
