"
" ~/.config/nvim/autocmds.vim
"

augroup vimWikiGroup
    command! Diary VimwikiDiaryIndex
    autocmd!
    " Automatically update links on read diary
    autocmd BufRead,BufNewFile diary.wiki VimwikiDiaryGenerateLinks
augroup end

" PHP File Types (WordPress, use tabs)
augroup phpGroup
    autocmd!
    autocmd FileType php set noexpandtab
augroup END

" Set up C build
augroup cGroup
    autocmd!
    autocmd Filetype c call COptions()
        " F10 to toggle quickfix window
    autocmd Filetype c nnoremap <F10> :call asyncrun#quickfix_toggle(6)<CR>
        " Run single file
    autocmd Filetype c noremap <silent> <F5> :AsyncRun -raw -cwd=$(VIM_FILEDIR) "$(VIM_FILEDIR)/BUILD/$(VIM_FILENOEXT)" <CR>
        " Build current project
    autocmd Filetype c noremap <silent> <F7> :AsyncRun -cwd=<root> make <cr>
        " Run current project
    autocmd Filetype c noremap <silent> <F8> :AsyncRun -cwd=<root> -raw make run <cr>
augroup END

" JS
augroup javaScriptGroup
    autocmd!
    autocmd Filetype js set noexpandtab
    autocmd Filetype js set wrap
    autocmd Filetype svelte set noexpandtab
augroup END

" Svelte
augroup svelteGroup
    autocmd!
    autocmd Filetype svelte set noexpandtab
augroup END

" Go
augroup golangGroup
    autocmd!
    autocmd Filetype go noremap <silent> <F5> :!clear; go run %<CR>
    autocmd Filetype go nnoremap <Leader>gof :GoFmt<CR>
    autocmd Filetype go nnoremap <Leader>gor :GoRename<CR>
    autocmd Filetype go nnoremap <Leader>god :GoDef<CR>
    autocmd Filetype go nnoremap <Leader>gob :GoRun<CR>
    autocmd Filetype go nnoremap <Leader>goe :GoIfErr<CR>
augroup END

" Disable CursorLine/-Column in inactive windows
augroup cursorLineGroup
    autocmd!
    autocmd VimEnter    * setlocal cursorline
    autocmd VimEnter    * setlocal cursorcolumn
    autocmd WinEnter    * setlocal cursorline
    autocmd WinEnter    * setlocal cursorcolumn
    autocmd BufWinEnter * setlocal cursorline
    autocmd BufWinEnter * setlocal cursorcolumn
    autocmd WinLeave    * setlocal nocursorline
    autocmd WinLeave    * setlocal nocursorcolumn
augroup END

autocmd BufEnter * setlocal formatoptions-=cro

" Remember position of last edit and return on reopen
if has("autocmd")
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

