set background=light
highlight clear
if exists("syntax_on")
	syntax reset
endif

hi clear Normal
" rgb=38,38,38
hi Normal     ctermbg=234
hi LineNr     ctermfg=Yellow
hi StatusLine ctermfg=Blue    ctermbg=White
hi spellbad   ctermbg=Red
hi Folded     ctermbg=LightGrey ctermfg=DarkBlue
hi StatusLine ctermfg=08 ctermbg=234

let colors_name = "myDef"
