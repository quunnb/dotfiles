--
-- ~/.config/nvim/lua/plugins/mini.lua
--

return {
  {
    'echasnovski/mini.nvim',
    config = function()
      -- Add animation
      require('mini.animate').setup()
      -- Move selection around
      require('mini.move').setup()
      -- Defaults:
      --   -- Module mappings. Use `''` (empty string) to disable one.
      --   mappings = {
      --     -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
      --     left = '<M-h>',
      --     right = '<M-l>',
      --     down = '<M-j>',
      --     up = '<M-k>',
      --
      --     -- Move current line in Normal mode
      --     line_left = '<M-h>',
      --     line_right = '<M-l>',
      --     line_down = '<M-j>',
      --     line_up = '<M-k>',
      --   },
      --
      --   -- Options which control moving behavior
      --   options = {
      --     -- Automatically reindent selection during linewise vertical move
      --     reindent_linewise = true,
      --   },
      -- }
      -- Better Around/Inside textobjects
      --
      -- Examples:
      --  - va)  - [V]isually select [A]round [)]paren
      --  - yinq - [Y]ank [I]nside [N]ext [Q]uote
      --  - ci'  - [C]hange [I]nside [']quote
      require('mini.ai').setup { n_lines = 500 }

      -- Add/delete/replace surroundings (brackets, quotes, etc.)
      --
      -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
      -- - sd'   - [S]urround [D]elete [']quotes
      -- - sr)'  - [S]urround [R]eplace [)] [']
      require('mini.surround').setup()
      -- Show buffers as a tabline
      require('mini.tabline').setup()
      -- Visualize indentation
      require('mini.indentscope').setup()
      -- Save sessions
      require('mini.sessions').setup()
      -- {
      --   -- Whether to read default session if Neovim opened without file arguments
      --   autoread = false,
      --
      --   -- Whether to write currently read session before quitting Neovim
      --   autowrite = true,
      --
      --   -- Directory where global sessions are stored (use `''` to disable)
      --   directory = --<"session" subdir of user data directory from |stdpath()|>,
      --
      --     -- File for local session (use `''` to disable)
      --     file = 'Session.vim',
      --
      --   -- Whether to force possibly harmful actions (meaning depends on function)
      --   force = { read = false, write = true, delete = false },
      --
      --   -- Hook functions for actions. Default `nil` means 'do nothing'.
      --   hooks = {
      --     -- Before successful action
      --     pre = { read = nil, write = nil, delete = nil },
      --     -- After successful action
      --     post = { read = nil, write = nil, delete = nil },
      --   },
      --
      --   -- Whether to print session path after action
      --   verbose = { read = false, write = true, delete = true },
      -- }
      -- }
      -- Align text
      require('mini.align').setup()
      --  Statusline
      local statusline = require 'mini.statusline'
      -- set use_icons to true if you have a Nerd Font
      statusline.setup { use_icons = vim.g.have_nerd_font }

      -- You can configure sections in the statusline by overriding their
      -- default behavior. For example, here we set the section for
      -- cursor location to LINE:COLUMN
      ---@diagnostic disable-next-line: duplicate-set-field
      statusline.section_location = function()
        return '%2l:%-2v'
      end

      -- ... and there is more!
      --  Check out: https://github.com/echasnovski/mini.nvim
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
