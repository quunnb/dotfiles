--
-- ~/.config/nvim/lua/plugins/substitute.lua
--

return {
  'gbprod/substitute.nvim',
  opts = {
    preserve_cursor_position = true,
  },
  config = function()
    require('substitute').setup {
      -- Substitute
      vim.keymap.set('n', 's', require('substitute').operator, { noremap = true }),
      vim.keymap.set('n', 'ss', require('substitute').line, { noremap = true }),
      vim.keymap.set('n', 'S', require('substitute').eol, { noremap = true }),
      vim.keymap.set('x', 's', require('substitute').visual, { noremap = true }),

      -- System clipboard
      vim.keymap.set({ 'n', 'x' }, '<leader>s', function()
        require('substitute').operator { register = '+' }
      end),
      { noremap = true },
      -- vim.keymap.set({ 'n', 'x' }, '<leader>ss', function()
      vim.keymap.set('x', '<leader>ss', function()
        require('substitute').line { register = '+' }
      end),
      { noremap = true },
      vim.keymap.set('n', '<leader>S', function()
        require('substitute').eol { register = '+' }
      end),
      { noremap = true },

      -- Exchange
      vim.keymap.set('n', 'sx', require('substitute.exchange').operator, { noremap = true }),
      vim.keymap.set('n', 'sxx', require('substitute.exchange').line, { noremap = true }),
      vim.keymap.set('x', 'X', require('substitute.exchange').visual, { noremap = true }),
      vim.keymap.set('n', 'sxc', require('substitute.exchange').cancel, { noremap = true }),
    }
  end,
}

-- vim: ts=2 sts=2 sw=2 et
