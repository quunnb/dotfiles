--
-- ~/.config/nvim/lua/plugins/yanky.lua
--

return {
  'gbprod/yanky.nvim',
  opts = {},
  config = function()
    require('yanky').setup {
      vim.keymap.set({ 'n', 'x' }, 'p', '<Plug>(YankyPutAfter)'),
      vim.keymap.set({ 'n', 'x' }, 'P', '<Plug>(YankyPutBefore)'),
      -- vim.keymap.set({ 'n', 'x' }, 'gp', '<Plug>(YankyGPutAfter)'),
      -- vim.keymap.set({ 'n', 'x' }, 'gP', '<Plug>(YankyGPutBefore)'),
      vim.keymap.set('n', '<C-p>', '<Plug>(YankyPreviousEntry)'),
      vim.keymap.set('n', '<C-n>', '<Plug>(YankyNextEntry)'),
    }
  end,
}

-- vim: ts=2 sts=2 sw=2 et
