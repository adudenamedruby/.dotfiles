return {
    "pwntester/octo.nvim",
    cmd = "Octo",
    opts = {
        -- or "fzf-lua" or "snacks" or "default"
        picker = "fzf-lua",
        -- bare Octo command opens picker of commands
        enable_builtin = true,
    },
    keys = function()
        local ok, wk = pcall(require, "which-key")
        if ok then
            wk.add({
                { "<leader>gc", group = "octo comments" },
                { "<leader>gr", group = "octo review" },
                { "<leader>gp", group = "octo PRs" },
                { "<leader>ge", group = "octo emoji reaction" },
                -- { "<leader>gL", group = "octo Label" },
            })
        end

        local U = require("adudenamedruby.core.utils")
        return {
            U.PLMap("gO", "<cmd>Octo<CR>", "octo options"),
            U.PLMap("gN", "<cmd>Octo notification list<CR>", "list GitHub notifications"),
            U.PLMap("gB", "<cmd>Octo repo browser<CR>", "open repo in browser"),

            -- review
            U.PLMap("grs", "<cmd>Octo review start<CR>", "start a review"),
            U.PLMap("grS", "<cmd>Octo review submit<CR>", "submit the review"),
            U.PLMap("grr", "<cmd>Octo review resume<CR>", "resume the review"),
            U.PLMap("grd", "<cmd>Octo review discard<CR>", "discard current review"),
            U.PLMap("grp", "<cmd>Octo review comments<CR>", "view pending comments"),
            U.PLMap("grC", "<cmd>Octo review commit<CR>", "pick a commit to review"),
            U.PLMap("grc", "<cmd>Octo review close<CR>", "close review window"),
            U.PLMap("grA", "<cmd>Octo reviewer add<CR>", "assign a reviewer"),

            -- comments
            U.PLMap("gca", "<cmd>Octo comment add<CR>", "add comment"),
            U.PLMap("gcs", "<cmd>Octo comment suggest<CR>", "suggest"),
            U.PLMap("gcd", "<cmd>Octo comment delete<CR>", "delete comment"),
            U.PLMap("gcr", "<cmd>Octo comment reply<CR>", "reply to comment"),

            -- prs
            U.PLMap("gpl", "<cmd>Octo pr list<CR>", "list GitHub PRs"),
            U.PLMap("gpR", "<cmd>Octo pr reopen<CR>", "reopen current PR"),
            U.PLMap("gpX", "<cmd>Octo pr close<CR>", "close the current PR"),
            U.PLMap("gpc", "<cmd>Octo pr changes<CR>", "show PR changes"),
            U.PLMap("gpC", "<cmd>Octo pr close<CR>", "show PR commits"),
            U.PLMap("gpo", "<cmd>Octo pr checkout<CR>", "checkout the current PR"),
            U.PLMap("gpd", "<cmd>Octo pr diff<CR>", "show PR diff"),
            U.PLMap("gpD", "<cmd>Octo pr draft<CR>", "send to draft"),
            U.PLMap("gpr", "<cmd>Octo pr reload<CR>", "reload pr"),
            U.PLMap("gpb", "<cmd>Octo pr browser<CR>", "open in browser"),
            U.PLMap("gpe", "<cmd>Octo pr edit<CR>", "edit PR #"),

            -- emoji reactions
            U.PLMap("ge+", "<cmd>Octo reaction thumbs_up<CR>", "Add 👍 reaction"),
            U.PLMap("ge-", "<cmd>Octo reaction thumbs_down<CR>", "Add 👎 reaction"),
            U.PLMap("gee", "<cmd>Octo reaction eyes<CR>", "Add 👀 reaction"),
            U.PLMap("gel", "<cmd>Octo reaction laugh<CR>", "Add 😄 reaction"),
            U.PLMap("gec", "<cmd>Octo reaction confused<CR>", "Add 😕 reaction"),
            U.PLMap("ger", "<cmd>Octo reaction rocket<CR>", "Add 🚀 reaction"),
            U.PLMap("geh", "<cmd>Octo reaction heart<CR>", "Add ❤️ reaction"),
            U.PLMap("gep", "<cmd>Octo reaction p<CR>", "Add 🎉 reaction"),
        }
    end,
    dependencies = {
        "nvim-lua/plenary.nvim",
        "ibhagwan/fzf-lua",
        "nvim-tree/nvim-web-devicons",
    },
}
