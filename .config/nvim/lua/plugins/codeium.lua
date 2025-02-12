return {
  'Exafunction/codeium.vim',
  config = function()
    -- Good
    vim.keymap.set('i', '<M-g>', function()
      return vim.fn['codeium#Accept']()
    end, { expr = true, silent = true })

    -- Next
    vim.keymap.set('i', '<M-,>', function()
      return vim.fn['codeium#CycleCompletions'](1)
    end, { expr = true, silent = true })

    -- Prev
    vim.keymap.set('i', '<M-;>', function()
      return vim.fn['codeium#CycleCompletions'](-1)
    end, { expr = true, silent = true })

    -- Clear
    vim.keymap.set('i', '<M-x>', function()
      return vim.fn['codeium#Clear']()
    end, { expr = true, silent = true })

    -- Enable
    vim.keymap.set('n', '<M-c>e', function()
      return vim.fn['codeium#Enable']()
    end, { expr = true, silent = true })

    -- Disable
    vim.keymap.set('n', '<M-c>d', function()
      return vim.fn['codeium#Disable']()
    end, { expr = true, silent = true })

    -- Chat
    vim.keymap.set('n', '<M-c>c', function()
      return vim.fn['codeium#Chat']()
    end, { expr = true, silent = true })
  end,
}
