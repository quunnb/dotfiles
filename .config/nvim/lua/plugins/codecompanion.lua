return {
  'olimorris/codecompanion.nvim',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-treesitter/nvim-treesitter',
  },
  opts = {
    strategies = {
      -- Change the default chat adapter
      chat = {
        adapter = 'mistral',
      },
    },
    opts = {
      -- Set debug logging
      -- log_level = "DEBUG",
    },
  },
}
