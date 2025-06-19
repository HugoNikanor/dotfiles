set tabstop=8

function! s:TZDBHiglights()
	highlight! TZDBKeyword     cterm=NONE ctermfg=03
	highlight! TZDBSpecialChar cterm=NONE ctermfg=12
endfunction

autocmd ColorScheme * call s:TZDBHiglights()
