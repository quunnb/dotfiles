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

" set list
set autochdir
set clipboard=unnamedplus
set expandtab
set shiftwidth=4
set sidescroll=8
set foldmethod=marker
set listchars=tab:>-,trail:·,nbsp:_
set nostartofline
set number
set relativenumber
set scrolloff=5
set shiftround
set ignorecase
set smartindent
set softtabstop=4
set tabstop=4
set wildmode=list:longest,list:full
set spelllang=en_us
set spell

colorscheme rosepine

" Use xsel if possible
let g:clipboard = {
      \   'name': 'xsel_override',
      \   'copy': {
      \      '+': 'xsel --input --clipboard',
      \      '*': 'xsel --input --primary',
      \    },
      \   'paste': {
      \      '+': 'xsel --output --clipboard',
      \      '*': 'xsel --output --primary',
      \   },
      \   'cache_enabled': 1,
      \ }
