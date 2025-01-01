--
-- ~/.config/nvim/lua/plugins/today.lua
--

return {
  'VVoruganti/today.nvim',
  config = function()
    require('today').setup()
  end,
}

-- vim: ts=2 sts=2 sw=2 et
