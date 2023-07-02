local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

require("adudenamedruby.lsp.mason")
require("adudenamedruby.lsp.handlers").setup()
require("adudenamedruby.lsp.null-ls")
