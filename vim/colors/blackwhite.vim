set background=dark
highlight clear
if exists("syntax_on")
	syntax reset
endif

let colors_name = "blackwhite"

"highlight Normal ctermfg=White ctermbg=Black guifg=White guibg=Black
highlight LineNr ctermfg=White
highlight CursorLineNr ctermfg=Black ctermbg=White

highlight ColorColumn ctermbg=White ctermfg=Black

highlight StatusLine ctermfg=White ctermbg=Black
highlight StatusLineNC ctermfg=White ctermbg=Black

highlight Folded ctermbg=NONE ctermfg=White
highlight VertSplit ctermbg=Black ctermfg=White

syntax off
