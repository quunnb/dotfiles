--
-- ~/.config/nvim/lua/plugins/hop.lua
--

return {
  'smoka7/hop.nvim',
  version = '*',
  opts = {
    keys = 'tnseriaoufywmcxjgzäöq',
  },
  config = function()
    require('hop').setup {
      quit_key = '<Esc>',
      -- case_insensitive = false,
      -- uppercase_labels = true,
      vim.keymap.set('n', '<Leader>w', ':HopWord<CR>', { noremap = true }),
      vim.keymap.set('n', '<Leader>h', ':HopChar1<CR>', { noremap = true }),
      vim.keymap.set('n', 'f', ':HopChar1CurrentLine<CR>', { noremap = true }),
      vim.keymap.set('n', 't', 'function() require"hop".hint_char1({ current_line_only = true, hint_offset = -1})', { noremap = true }),
      vim.keymap.set(
        'n',
        'F',
        'function() require"hop".hint_char1({ direction = require"hop".hint.HintDirection.BEFORE_CURSOR, current_line_only = true })',
        { noremap = true }
      ),
      vim.keymap.set(
        'n',
        'T',
        'function() require"hop".hint_char1({ direction = require"hop".hint.HintDirection.BEFORE_CURSOR, current_line_only = true, hint_offset = 1})',
        { noremap = true }
      ),
    }
  end,
}
