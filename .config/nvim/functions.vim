"
" ~/.config/nvim/functions.vim
"

" Delete closed captions
function! DeleteCC()
    :%s/[ -]\{,2}\[.*\] \?//g<CR>
endfunction

" Set C build
function! COptions()
    setlocal commentstring=//\ %s
    " Try to find the build file
    if filereadable(expand("./build.sh"))
        setlocal makeprg=./build.sh\ %
    elseif filereadable(expand("~/dev/c/build.sh"))
        setlocal makeprg=~/dev/c/build.sh\ %
    endif
endfunction

" Replace colorname under cursor with value from .Xresources
function! GetXresColor()
    let l:hex = substitute(system('xrdb -query | grep ' . expand("<cword>") . ' | cut -f 2'), '\n\+$', '', '')
    execute "normal! ciw" . l:hex
endfunction

" Replace some colors (in some wikipedia svg maps)
function! ReplaceColors()
    :%s/#C6ECFF/#1f1d2e/ig " sea
    :%s/#B2DDF2/#1f1d2e/ig
    :%s/#B0DBF1/#1f1d2e/ig
    :%s/#A9D6EB/#1f1d2e/ig
    :%s/#9ECCE3/#1f1d2e/ig
    :%s/#8DBFD6/#1f1d2e/ig
    :%s/#77ADC5/#1f1d2e/ig
    :%s/#5B97B1/#1f1d2e/ig
    :%s/#3B7D98/#1f1d2e/ig
    :%s/#17607D/#1f1d2e/ig
    :%s/#024F6D/#1f1d2e/ig
    :%s/#fff/#1f1d2e/ig
    :%s/#FDFBE5/#26233a/ig " land
    :%s/#e0e0e0/#26233a/ig " land
    :%s/#F9FAE6/#1f1d2e/ig
    :%s/#F7F8E4/#1f1d2e/ig
    :%s/#F0F1DF/#1f1d2e/ig
    :%s/#E5E6D4/#1f1d2e/ig
    :%s/#D4D5C6/#1f1d2e/ig
    :%s/#BEBFB3/#1f1d2e/ig
    :%s/#A2A39B/#1f1d2e/ig
    :%s/#838480/#1f1d2e/ig
    :%s/#656666/#1f1d2e/ig
    :%s/#336733/#9ccfd8/ig " highlighted land
    :%s/#73cd73/#c4a7e7/ig " disputed
    :%s/#335033/#9ccfd8/ig " highlighted borders
    :%s/#646464/#ebbcba/ig " borders
    :%s/#0978AB/#ebbcba/ig " coast
    :%s/#C12838/#9ccfd8/ig " focus
    :%s/#FFFFFF/#ebbcba/ig " horizon

    :%s/#F7DC6F/#f6c177/ig
    :%s/#F0B27A/#ebbcba/ig
    :%s/#D2B4DE/#c4a7e7/ig
    :%s/#FF4500/#eb6f92/ig
    :%s/#ADD8E6/#9ccfd8/ig
    :%s/#FFFF00/#f6c177/ig
    :%s/#A020F0/#c4a7e7/ig
    :%s/#A545FF/#c4a7e7/ig
    :%s/#FFC0CB/#ebbcba/ig
    :%s/#000000/#191724/ig
endfunction

