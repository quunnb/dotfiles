"
" ~/.config/init.vim
"

filetype on
filetype plugin on
filetype indent on
syntax on

let g:vim_home = get(g:, 'vim_home', expand('~/.config/nvim/'))

let config_list = [
      \ 'plugins.vim',
      \ 'functions.vim',
      \ 'mappings.vim',
      \ 'autocmds.vim',
      \ 'abbreviations.vim',
      \ 'plugin_settings/*.vim'
      \]

for files in config_list
  for f in glob(g:vim_home.files, 1, 1)
    exec 'source' f
  endfor
endfor

if has("persistent_undo")
   let target_path = expand('~/.config/nvim/undo')
    if !isdirectory(target_path)
        call mkdir(target_path, "p", 0700)
    endif
    let &undodir=target_path
    set undofile
endif

set clipboard=unnamedplus
set foldmethod=marker
set list
set nostartofline
set number
set relativenumber
set sidescroll=8
set autochdir
set wildmode=list:longest,list:full

colorscheme rosepine
