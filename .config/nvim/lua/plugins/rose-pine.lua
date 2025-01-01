--
-- ~/.config/nvim/lua/plugins/rose-pine.lua
--

return {
  {
    'rose-pine/neovim',
    name = 'rose-pine',
    priority = 1000, -- Make sure to load this before all the other start plugins.
    disable_background = true,
    init = function()
      vim.cmd.colorscheme 'rose-pine'
      vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
      vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
