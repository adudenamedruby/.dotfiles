return {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = { "BufReadPre", "BufNewFile" },
    keys = function()
        local U = require("adudenamedruby.core.utils")

        return {
            U.PLMap("tb", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", "buffer"),
            U.PLMap("tp", "<cmd>Trouble diagnostics toggle<CR>", "project"),
            U.PLMap("tS", "<cmd>Trouble symbols toggle<CR>", "symbols"),
            U.PLMap("tt", "<cmd>Trouble lsp_type_definitions toggle<CR>", "lsp type definitions"),
            U.PLMap("tr", "<cmd>Trouble lsp_references toggle<CR>", "lsp references"),
            U.PLMap("ts", "<cmd>Trouble lsp_document_symbols toggle<CR>", "lsp symbols"),
            U.PLMap("td", "<cmd>Trouble lsp_definitions toggle<CR>", "lsp definitions"),
            U.PLMap("tD", "<cmd>Trouble lsp_declarations toggle<CR>", "lsp declarations"),
            U.PLMap("tl", "<cmd>Trouble lsp toggle<CR>", "lsp items"),
            U.PLMap("tL", "<cmd>Trouble loclist toggle<cr>", "loclist"),
            U.PLMap("tq", "<cmd>Trouble quickfix toggle<cr>", "quickfix"),
        }
    end,
    config = function()
        require("trouble").setup({
            focus = true,
        })

        local harpoon = require("trouble")
        harpoon.setup({})

        local ok, wk = pcall(require, "which-key")
        if ok then
            wk.add({
                { "<leader>t", group = "trouble" },
            })
        end
    end,
}
