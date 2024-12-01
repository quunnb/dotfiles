command! Diary VimwikiDiaryIndex
augroup vimwikigroup
    autocmd!
    " Automatically update links on read diary
    autocmd BufRead,BufNewFile diary.wiki VimwikiDiaryGenerateLinks
augroup end

" PHP File Types (WordPress, use tabs)
autocmd FileType php set noexpandtab

" Set up C build
autocmd Filetype c call COptions()
    " F10 to toggle quickfix window
autocmd Filetype c nnoremap <F10> :call asyncrun#quickfix_toggle(6)<CR>
    " Run single file
autocmd Filetype c noremap <silent> <F5> :AsyncRun -raw -cwd=$(VIM_FILEDIR) "$(VIM_FILEDIR)/BUILD/$(VIM_FILENOEXT)" <CR>
    " Build current project
autocmd Filetype c noremap <silent> <F7> :AsyncRun -cwd=<root> make <cr>
    " Run current project
autocmd Filetype c noremap <silent> <F8> :AsyncRun -cwd=<root> -raw make run <cr>

" JS
autocmd Filetype js set noexpandtab
autocmd Filetype js set wrap

" Svelte
autocmd Filetype svelte set noexpandtab

" Remember position of last edit and return on reopen
if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Go
autocmd Filetype go noremap <silent> <F5> :!clear; go run %<CR>

" Disable CursorLine/-Column in inactive windows
augroup CursorLine
    au!
    au VimEnter    * setlocal cursorline
    au VimEnter    * setlocal cursorcolumn
    au WinEnter    * setlocal cursorline
    au WinEnter    * setlocal cursorcolumn
    au BufWinEnter * setlocal cursorline
    au BufWinEnter * setlocal cursorcolumn
    au WinLeave    * setlocal nocursorline
    au WinLeave    * setlocal nocursorcolumn
augroup END

" Generic stuff at BufEnter
au BufEnter * setlocal formatoptions-=cro

" Start NERDTree and put the cursor back in the other window.
" autocmd VimEnter * NERDTree | wincmd p
