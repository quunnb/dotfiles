" 
" ~/.config/nvim/mappings.vim
" 

let mapleader = " "

" Y you have to be such a snowflake
nnoremap Y y$

" run q macro
nnoremap Q @q

" Move per visual line
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Move per real line
nnoremap gj j
nnoremap gk k
vnoremap gj j
vnoremap gk k

" Keep visual selection
xnoremap > >gv
xnoremap < <gv
xnoremap <C-a> <C-a>gv
xnoremap <C-x> <C-x>gv

" For Finnish keyboards
noremap , ;
noremap ; ,

" Toggle show whitespace
nnoremap <F3> :set list!<CR>
"Remove all trailing whitespace
nnoremap <F4> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>

" Disable/enable automatic comments
nnoremap <Leader>com :set formatoptions-=cro<CR>
nnoremap <Leader>C :set formatoptions+=cro<CR>
inoremap <C-c> <Esc>:set formatoptions-=cro<CR>a<Space>

" Hide search highlights
nnoremap <Leader><Leader> :nohls<CR>

" Open / reload (source) vimrc
nnoremap <Leader>vi :e $MYVIMRC<CR>
nnoremap <Leader>re :source $MYVIMRC<CR>

" Fix spelling
inoremap <C-s> <Esc>:set spell<CR>a<C-x><C-s>
" Autocomplete line
inoremap <C-l> <C-x><C-l>

" Keep search matches in the middle of the window
nnoremap n nzz
nnoremap N Nzz

" Open non-existing file
noremap gf :e <cfile><CR>

" Insert current path
nnoremap <Leader>p :put=expand('%:p:~')<CR>
nnoremap <Leader>P :put=expand('%:p')<CR>

" Hic sunt dracones
cmap Q<CR> q<CR>
nnoremap Q <NOP>

" Buffer navigation
nnoremap <Leader>j :bn<CR>
nnoremap <Leader>k :bp<CR>
nnoremap <Leader>d :bd<CR>

" Get color hex value from .Xresources and replace color word under cursor
nnoremap <Leader>x :call GetXresColor()<CR>
xnoremap <Leader>x :call GetXresColor()<CR>

" Readline mappings
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-d> <Delete>

" Insert newline
nnoremap <CR> mzo<Esc>g`z<Down>
" Insert newline above
nnoremap <Leader><CR> mzO<Esc>g`z

" Upper-/lowercase word
nnoremap <Leader>u guiw
nnoremap <Leader>U gUiw

" Insert datetime
nnoremap <Leader>date :put =strftime('%d.%m.%Y %H:%M')<CR>

" Open terminal
nnoremap <Leader>t :vertical terminal<CR>

" Inc-/decrement hex or alpha characters
nnoremap <Leader>ia :set nrformats+=alpha<CR>
nnoremap <Leader>iA :set nrformats-=alpha<CR>
nnoremap <Leader>ih :set nrformats+=hex<CR>
nnoremap <Leader>iH :set nrformats-=hex<CR>

" Don't close accidentally
nnoremap ZZ :q

" Erase redundant spaces"
xnoremap <Leader>_ :s/ \+/ /g<CR>gv:s/ $//g<CR>

" Sort selection
xnoremap <Leader>so :sort<CR>
xnoremap <Leader>sn :sort n<CR>

" "VIM-SURROUND"
nnoremap <Leader>' :execute "normal \<Plug>YsurroundiW'"<CR>
nnoremap <Leader>" :execute "normal \<Plug>YsurroundiW\""<CR>
nnoremap <Leader>) :execute "normal \<Plug>YsurroundiW)"<CR>
nnoremap <Leader>( :execute "normal \<Plug>YsurroundiW("<CR>
nnoremap <Leader>] :execute "normal \<Plug>YsurroundiW]"<CR>
nnoremap <Leader>[ :execute "normal \<Plug>YsurroundiW["<CR>
nnoremap <Leader>} :execute "normal \<Plug>YsurroundiW}"<CR>
nnoremap <Leader>{ :execute "normal \<Plug>YsurroundiW{"<CR>

" "EASY-ALIGN"
nmap <Leader>a <Plug>(EasyAlign)
xmap <Leader>a <Plug>(EasyAlign)
nmap <Leader>a:k mzvi}:EasyAlign : { 'stick_to_left': 0, 'left_margin': 1 }<CR>`z
nmap <Leader>a:h mzvi}:EasyAlign : { 'stick_to_left': 1, 'left_margin': 0 }<CR>`z
nmap <Leader>a=k mzvi}:EasyAlign = { 'stick_to_left': 0, 'left_margin': 1 }<CR>`z
nmap <Leader>a=h mzvi}:EasyAlign = { 'stick_to_left': 1, 'left_margin': 0 }<CR>`z

" "SUBVERSIVE"
nmap s           <Plug>(SubversiveSubstitute)
nmap S           <Plug>(SubversiveSubstituteToEndOfLine)
nmap ss          <Plug>(SubversiveSubstituteLine)
nmap <Leader>s   <Plug>(SubversiveSubvertRange)
nmap <Leader>ss  <Plug>(SubversiveSubvertWordRange)
nmap <Leader>cs  <Plug>(SubversiveSubstituteRangeConfirm)
nmap <Leader>css <Plug>(SubversiveSubstituteWordRangeConfirm)
xmap <Leader>s   <Plug>(SubversiveSubvertRange)
xmap s           <Plug>(SubversiveSubstitute)
xmap p           <Plug>(SubversiveSubstitute)
xmap P           <Plug>(SubversiveSubstitute)
xmap <Leader>cs  <Plug>(SubversiveSubstituteRangeConfirm)

" "YOINK"
nmap <C-n>  <Plug>(YoinkPostPasteSwapBack)
nmap <C-p>  <Plug>(YoinkPostPasteSwapForward)
nmap p      <Plug>(YoinkPaste_p)
nmap P      <Plug>(YoinkPaste_P)
nmap gp     <Plug>(YoinkPaste_gp)
nmap gP     <Plug>(YoinkPaste_gP)

" "COLORIZER"
nnoremap <Leader>col :ColorToggle<CR>

" "FZF"
nnoremap <Leader>f :call fzf#run({'sink': 'e'})<CR>
nnoremap <Leader>F :FZF ~<CR>

" "ALE"
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
nnoremap <Leader>A :ALEToggle<CR>
nnoremap <Leader>D :ALEDetail<CR>

" "nnn"
nnoremap <Leader>o :NnnPicker %:p:h<CR>

" "RADICAL defaults"
" nmap gA <Plug>RadicalView
" xmap gA <Plug>RadicalView
nmap <Leader>dec <Plug>RadicalCoerceToDecimal
nmap <Leader>hex <Plug>RadicalCoerceToHex
nmap <Leader>oct <Plug>RadicalCoerceToOctal
nmap <Leader>bin <Plug>RadicalCoerceToBinary

" "EXCHANGE defaults"
" nremap cx  <Plug>Exchange
" nremap cxx <Plug>Exchange_line
" xremap X   <Plug>Exchange_selection

" "VIM-COMMENTARY defaults"
" gcc Comment out a line
" gc  Comment out a selection / motion

" "VIM-TITLECASE defaults"
" nmap <Leader>gz  <Plug>Titlecase
" nmap <Leader>gzz <Plug>TitlecaseLine
" vmap <Leader>gz  <Plug>Titlecase

" "Codeium"
nnoremap <Leader>cod :CodeiumToggle<CR>
nnoremap <Leader>cha :CodeiumChat<CR>
imap <C-,> <Cmd>call codeium#CycleCompletions(1)<CR>
imap <C-.> <Cmd>call codeium#CycleCompletions(-1)<CR>

" "Nerdtree"
nnoremap <C-t> :NERDTreeToggle<CR>

" "Goyo"
nnoremap <Leader>go :Goyo<CR>
