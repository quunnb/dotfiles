" 
" ~/.config/nvim/mappings.vim
" 

let mapleader = " "

" Y you have to be such a snowflake
nnoremap Y y$

" Run q macro
nnoremap Q @q

" You always want to go to the right column, no?
nnoremap ' `

" Keep cursorlike in the middle when moving
nnoremap <C-f> <C-f>zz
nnoremap <C-b> <C-b>zz
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" Keep the position after past
nmap yyp yymzp`z<Down>
nmap yyP yymzP`z<Down>

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

" Keep visual selection when indenting and inc-/decrementing
xnoremap > >gv
xnoremap < <gv
xnoremap <C-a> <C-a>gv
xnoremap <C-x> <C-x>gv

" Move selected lines
xnoremap J :m '>+1<CR>gv=gv
xnoremap K :m '<-2<CR>gv=gv

" For Finnish keyboards more logical movement direction
noremap , ;
noremap ; ,

" Toggle show whitespace
nnoremap <F3> :set list!<CR>
"Remove all trailing whitespace
nnoremap <F4> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>
" Remove extra spaces from selection / line
nnoremap <Leader>xs :s/\S\zs\s\{2,}/ /g<CR> :s/ \([,\.]\)/\1/g<CR>
xnoremap <Leader>xs :s/\S\zs\s\{2,}/ /g<CR>gv :s/ \([,\.]\)/\1/g<CR>

" Disable/enable automatic comments
nnoremap <Leader>com :set formatoptions-=cro<CR>
nnoremap <Leader>C :set formatoptions+=cro<CR>
inoremap <C-c> <Cmd>set formatoptions-=cro<CR>a<Space>

" Hide search highlights
nnoremap <Leader><Leader> :nohls<CR>

" Open / source vimrc and such
nnoremap <Leader>vim :e $MYVIMRC<CR>
nnoremap <Leader>viso :so $MYVIMRC<CR>
nnoremap <Leader>map :e ~/.config/nvim/mappings.vim<CR>
nnoremap <Leader>plug :e ~/.config/nvim/plugins.vim<CR>

" Set spell automagically when wanting to autocomplete
inoremap <C-s> <Cmd>set spell<CR><C-x><C-s>
inoremap <C-x><C-s> <Cmd>set spell<CR><C-x><C-s>
inoremap <C-x>s <Cmd>set spell<CR><C-x>s

" Autocomplete line
inoremap <C-l> <C-x><C-l>

" Keep search matches in the middle of the window
nnoremap n nzzzv
nnoremap N Nzzzv

" Insert current path
nnoremap <Leader>p :put=expand('%:p:~')<CR>
nnoremap <Leader>P :put=expand('%:p')<CR>

" Buffer navigation
nnoremap <Leader>j :bn<CR>
nnoremap <Leader>k :bp<CR>
nnoremap <Leader>dd :bd<CR>

" Get color hex value from .Xresources and replace color word under cursor
nnoremap <Leader>x :call GetXresColor()<CR>
xnoremap <Leader>x :call GetXresColor()<CR>

" Readline mappings
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-d> <Delete>

" Transpose words (Requires 
nmap <M-t> cxiwgecxiw
imap <M-t> <Esc>cxiwgecxiwa
" Transpose characters
imap <C-t> <ESC><Left>xpa

" Upper-/lowercase word (emacs bindings)
nnoremap <M-l> gue
nnoremap <M-u> gUe

" Flip array indice with the previous
nmap <M-]>t cxi]F]cxi]
imap <M-]>t <Esc>cxi]F]cxi]a
" testing[y][x] -> testing[y][x]

" Insert date and time
nnoremap <Leader>da :put =strftime('%d.%m.%Y')<CR>
nnoremap <Leader>ti :put =strftime('%H:%M')<CR>

" Open terminal
nnoremap <Leader>te :vertical terminal<CR>

" Enable/disable hex or alpha characters increment/decrement
nnoremap <Leader>ia :set nrformats+=alpha<CR>
nnoremap <Leader>iA :set nrformats-=alpha<CR>
nnoremap <Leader>ih :set nrformats+=hex<CR>
nnoremap <Leader>iH :set nrformats-=hex<CR>

" Don't close accidentally
nnoremap ZZ :q

" Erase redundant spaces"
xnoremap <Leader>_ :s/ \+/ /g<CR>gv:s/ $//g<CR>

" Sort selection alphabetically or numerically
xnoremap <Leader>so :sort<CR>
xnoremap <Leader>sn :sort n<CR>


" "VIM-SURROUND"
nnoremap <Leader>' :execute "normal \<Plug>YsurroundiW'"<CR>
nnoremap <Leader>` :execute "normal \<Plug>YsurroundiW`"<CR>
nnoremap <Leader>" :execute "normal \<Plug>YsurroundiW\""<CR>
nnoremap <Leader>) :execute "normal \<Plug>YsurroundiW)"<CR>
nnoremap <Leader>( :execute "normal \<Plug>YsurroundiW("<CR>
nnoremap <Leader>] :execute "normal \<Plug>YsurroundiW]"<CR>
nnoremap <Leader>[ :execute "normal \<Plug>YsurroundiW["<CR>
nnoremap <Leader>} :execute "normal \<Plug>YsurroundiW}"<CR>
nnoremap <Leader>{ :execute "normal \<Plug>YsurroundiW{"<CR>

xnoremap <Leader>' :execute "normal \<Plug>YsurroundiW'"<CR>
xnoremap <Leader>` :execute "normal \<Plug>YsurroundiW`"<CR>
xnoremap <Leader>" :execute "normal \<Plug>YsurroundiW\""<CR>
xnoremap <Leader>) :execute "normal \<Plug>YsurroundiW)"<CR>
xnoremap <Leader>( :execute "normal \<Plug>YsurroundiW("<CR>
xnoremap <Leader>] :execute "normal \<Plug>YsurroundiW]"<CR>
xnoremap <Leader>[ :execute "normal \<Plug>YsurroundiW["<CR>
xnoremap <Leader>} :execute "normal \<Plug>YsurroundiW}"<CR>
xnoremap <Leader>{ :execute "normal \<Plug>YsurroundiW{"<CR>

" "EASY-ALIGN"
nmap <Leader>a <Plug>(EasyAlign)
xmap <Leader>a <Plug>(EasyAlign)
nmap <Leader>a:k mzvi}:EasyAlign : { 'stick_to_left': 0, 'left_margin': 1 }<CR>`z
nmap <Leader>a:: mzvi}:EasyAlign : { 'stick_to_left': 0, 'left_margin': 1 }<CR>`z
xmap <Leader>a:: :EasyAlign : { 'stick_to_left': 0, 'left_margin': 1 }<CR>`z

nmap <Leader>a:h mzvi}:EasyAlign : { 'stick_to_left': 1, 'left_margin': 0 }<CR>`z
xmap <Leader>a:h :EasyAlign : { 'stick_to_left': 1, 'left_margin': 0 }<CR>

nmap <Leader>a=k mzvi}:EasyAlign = { 'stick_to_left': 0, 'left_margin': 1 }<CR>`z
nmap <Leader>a== mzvi}:EasyAlign = { 'stick_to_left': 0, 'left_margin': 1 }<CR>`z
xmap <Leader>a== :EasyAlign = { 'stick_to_left': 0, 'left_margin': 1 }<CR>

nmap <Leader>a=h mzvi}:EasyAlign = { 'stick_to_left': 1, 'left_margin': 0 }<CR>`z
xmap <Leader>a=h :EasyAlign = { 'stick_to_left': 1, 'left_margin': 0 }<CR>


" "SUBVERSIVE"
nmap s <Plug>(SubversiveSubstitute)
nmap S <Plug>(SubversiveSubstituteToEndOfLine)
nmap ss <Plug>(SubversiveSubstituteLine)
nmap <Leader>s <Plug>(SubversiveSubvertRange)
nmap <Leader>ss <Plug>(SubversiveSubvertWordRange)
nmap <Leader>cs <Plug>(SubversiveSubstituteRangeConfirm)
nmap <Leader>css <Plug>(SubversiveSubstituteWordRangeConfirm)
xmap <Leader>s <Plug>(SubversiveSubvertRange)
xmap s <Plug>(SubversiveSubstitute)
xmap p <Plug>(SubversiveSubstitute)
xmap P <Plug>(SubversiveSubstitute)
xmap <Leader>cs <Plug>(SubversiveSubstituteRangeConfirm)

" "YOINK"
nmap <C-n> <Plug>(YoinkPostPasteSwapBack)
nmap <C-p> <Plug>(YoinkPostPasteSwapForward)
nmap p <Plug>(YoinkPaste_p)
nmap yyp yymz<Plug>(YoinkPaste_p)`z<Down>
nmap yyP yymz<Plug>(YoinkPaste_P)`z<Down>
nmap yyP <Plug>(YoinkPaste_P)
nmap gp <Plug>(YoinkPaste_gp)
nmap gP <Plug>(YoinkPaste_gP)

" "COLORIZER"
nnoremap <Leader>co :ColorToggle<CR>

" "ALE"
nmap <silent> <C-k>     <Plug>(ale_previous_wrap)
nmap <silent> <C-j>     <Plug>(ale_next_wrap)
nmap          <Leader>A <Plug>(ale_toggle)
nmap          <Leader>D <Plug>(ale_detail)
nmap          gt <Plug>(ale_go_to_type_definition)
nmap          gd <Plug>(ale_go_to_definition)
nmap          gr <Plug>(ale_find_references)
nmap          <Leader>r <Plug>(ale_rename)

" "RADICAL"
" nmap gA <Plug>RadicalView
" xmap gA <Plug>RadicalView
nmap <Leader>de <Plug>RadicalCoerceToDecimal
nmap <Leader>he <Plug>RadicalCoerceToHex
nmap <Leader>oc <Plug>RadicalCoerceToOctal
nmap <Leader>bi <Plug>RadicalCoerceToBinary

" "EXCHANGE"
" nremap cx  <Plug>Exchange
" nremap cxx <Plug>Exchange_line
" xremap X   <Plug>Exchange_selection

" "VIM-COMMENTARY"
" gcc Comment out a line
" gc  Comment out a selection / motion

" "VIM-TITLECASE"
" nmap <Leader>gz  <Plug>Titlecase
" nmap <Leader>gzz <Plug>TitlecaseLine
" vmap <Leader>gz  <Plug>Titlecase
" TODO: cycle between camel, snake and Pascal case with one binding
nnoremap <silent> <Leader>camel mq:s/_\(\w\)/\U\1/g<CR>g`q
nnoremap <silent> <Leader>snake mq:s/\([A-Z]\)/_\L\1/gI<CR>g`q

" "Codeium"
nnoremap <Leader>cod :CodeiumToggle<CR>
nnoremap <Leader>cha :CodeiumChat<CR>
imap <M-.> <Cmd>call codeium#CycleOrComplete()<CR>
imap <M-,> <Cmd>call codeium#CycleCompletions(-1)<CR>

" "Goyo"
nnoremap <Leader>goyo :Goyo<CR>
