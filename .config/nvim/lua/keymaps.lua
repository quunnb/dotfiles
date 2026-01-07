-- [[ Basic Keymaps ]]
--  See `:help vim.keymap.set()`

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
-- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

--  See `:help wincmd` for a list of all window commands
vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- NOTE: Some terminals have colliding keymaps or are not able to send distinct keycodes
-- vim.keymap.set("n", "<C-S-h>", "<C-w>H", { desc = "Move window to the left" })
-- vim.keymap.set("n", "<C-S-l>", "<C-w>L", { desc = "Move window to the right" })
-- vim.keymap.set("n", "<C-S-j>", "<C-w>J", { desc = "Move window to the lower" })
-- vim.keymap.set("n", "<C-S-k>", "<C-w>K", { desc = "Move window to the upper" })

-- Always jump to the marked character
vim.keymap.set('n', "'", '`')

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>td', function()
  vim.diagnostic.enable(not vim.diagnostic.is_enabled())
end, { silent = true, noremap = true })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })
vim.keymap.set('n', '<leader>od', vim.diagnostic.open_float, { desc = '[O]pen [D]iagnostic' })
vim.keymap.set('n', '<C-k>', vim.diagnostic.goto_next, { desc = 'Goto next diagnosis' })
vim.keymap.set('n', '<C-j>', vim.diagnostic.goto_prev, { desc = 'Goto prev diagnosis' })

-- Remove annoying mapping
vim.keymap.set('n', 'Q', '<Nop>', { noremap = true })

-- Keep it centered
vim.keymap.set('n', 'n', 'nzz', { noremap = true })
vim.keymap.set('n', 'N', 'Nzz', { noremap = true })
vim.keymap.set('n', 'J', 'Jzz', { noremap = true })
vim.keymap.set('n', '<C-i>', '<C-i>zz', { noremap = true })
vim.keymap.set('n', '<C-o>', '<C-o>zz', { noremap = true })
vim.keymap.set('n', '<C-d>', '<C-d>zz', { noremap = true })
vim.keymap.set('n', '<C-u>', '<C-u>zz', { noremap = true })
vim.keymap.set('n', '<C-f>', '<C-f>zz', { noremap = true })
vim.keymap.set('n', '<C-b>', '<C-b>zz', { noremap = true })
vim.keymap.set('n', '{', '{zz', { noremap = true })
vim.keymap.set('n', '}', '}zz', { noremap = true })
vim.keymap.set('n', '(', '(zz', { noremap = true })
vim.keymap.set('n', ')', ')zz', { noremap = true })

-- Add relative jumps to jumplist
vim.keymap.set('n', 'k', '(v:count > 5 ? "m\'" . v:count : "") . \'k\'', { noremap = true, expr = true })
vim.keymap.set('n', 'j', '(v:count > 5 ? "m\'" . v:count : "") . \'j\'', { noremap = true, expr = true })

-- Sensible horizontal movement for Finnish keyboards
vim.keymap.set({ 'n', 'x' }, ',', ';')
vim.keymap.set({ 'n', 'x' }, ';', ',')

-- Move by visual line
vim.keymap.set({ 'n', 'x' }, 'j', 'gj')
vim.keymap.set({ 'n', 'x' }, 'k', 'gk')
vim.keymap.set({ 'n', 'x' }, 'gj', 'j')
vim.keymap.set({ 'n', 'x' }, 'gk', 'k')

-- Go to & delete buffers
vim.keymap.set('n', '<leader>j', ':bn<CR>')
vim.keymap.set('n', '<leader>k', ':bp<CR>')
vim.keymap.set('n', '<leader>d', ':bd<CR>')

-- Keep highlighting and selection when indenting and incrementing
vim.keymap.set('x', '<', '<gv')
vim.keymap.set('x', '>', '>gv')
vim.keymap.set('x', '<C-a>', '<C-a>gv')
vim.keymap.set('x', '<C-x>', '<C-x>gv')
vim.keymap.set('x', 'g<C-a>', 'g<C-a>gv')
vim.keymap.set('x', 'g<C-x>', 'g<C-x>gv')
--
-- Insert relative/absolute path
vim.keymap.set('n', '<leader>ip', ":put=expand('%:p:~')<CR>")
vim.keymap.set('n', '<leader>iP', ":put=expand('%:p')<CR>")
-- Insert date and time
vim.keymap.set('n', '<leader>id', ":put =strftime('%d.%m.%Y')<CR>")
vim.keymap.set('n', '<leader>it', ":put =strftime('%H:%M')<CR>")
-- Paste from the unnamed register
vim.keymap.set({ 'c', 'i', 't' }, '<C-r><C-r>', '<C-r>"')
-- Evaluate selection
vim.keymap.set('x', '<M-=>', '"zyA = <C-r>=<C-r>z<CR><Esc>')
vim.keymap.set('x', '<leader>=', '"zyA = <C-r>=<C-r>z<CR><Esc>')
-- Evaluate line in insert mode
vim.keymap.set('i', '<M-=>', '<Esc>"zyyA= <C-r>=<C-r>z<CR>')

-- Traditional readline bindings
vim.keymap.set({ 'c', 't' }, '<C-a>', '<Home>')
vim.keymap.set({ 'c', 't' }, '<C-e>', '<End>')
vim.keymap.set({ 'c', 't' }, '<C-b>', '<Left>')
vim.keymap.set({ 'c', 't' }, 'C-f', '<Right>')
vim.keymap.set({ 'c', 't' }, '<M-b>', '^<Left>')
vim.keymap.set({ 'c', 't' }, '<M-f>', '^<Right>')

-- UPPER-/lowercase word (emacs bindings)
vim.keymap.set({ 'n', 'x' }, '<M-l>', 'gue')
vim.keymap.set({ 'n', 'x' }, '<M-u>', 'gUe')

-- Sort selection
vim.keymap.set('x', '<leader>so', ':sort<CR>')
-- Sort numerically
vim.keymap.set('x', '<leader>sn', ':sort n<CR>')

-- Format indentation and keep position
vim.keymap.set('n', '==', 'mzgg=G`z')
vim.keymap.set({ 'n', 'x' }, '=}', 'mz=i}`z')
vim.keymap.set({ 'n', 'x' }, '={', 'mz=i{`z')

-- Window resize
vim.keymap.set('n', '<M-h>', '<C-w><')
vim.keymap.set('n', '<M-l>', '<C-w>>')
vim.keymap.set('n', '<M-j>', '<C-w>+')
vim.keymap.set('n', '<M-k>', '<C-w>-')

-- "Translate" (flip) words (like emacs)
-- TODO:

-- [[ Basic Autocommands ]]
--  See `:help lua-guide-autocommands`

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.hl.on_yank()`
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})

-- vim: ts=2 sts=2 sw=2 et
