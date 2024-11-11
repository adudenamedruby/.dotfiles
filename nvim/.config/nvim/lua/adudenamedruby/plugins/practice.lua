return {
    "doriancmore/practice.nvim",
    config = function()
        local practice = require('practice')
        practice.setup()
        KMap("<leader>tP", function() practice.open(10) end, "Practice NVim")
    end
}
