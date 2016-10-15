" https://gist.github.com/hkmix/41492855c3fcc7a9393b

set background=dark
highlight clear
if exists("syntax_on")
    syntax reset
endif

let colors_name = "comments-simple"

" First set Normal to regular white on black text colors:
"highlight Normal ctermfg=LightGray ctermbg=Black guifg=#dddddd guibg=Black
"highlight Normal ctermfg=LightGrey ctermbg=235 guifg=#dddddd guibg=#505050
"highlight Normal ctermfg=LightGrey ctermbg=234 guifg=#dddddd guibg=#505050
highlight Normal ctermfg=NONE ctermbg=NONE guifg=#dddddd guibg=#505050

" Syntax highlighting (other color-groups using default, see :help group-name):
highlight Comment      cterm=NONE        ctermfg=08
highlight Constant     cterm=NONE        ctermfg=NONE
highlight Identifier   cterm=NONE        ctermfg=NONE
highlight Function     cterm=NONE        ctermfg=NONE
highlight Statement    cterm=NONE        ctermfg=NONE
highlight PreProc      cterm=NONE        ctermfg=NONE
highlight Type         cterm=NONE        ctermfg=NONE
highlight Special      cterm=NONE        ctermfg=NONE
highlight Delimiter    cterm=NONE        ctermfg=NONE
highlight LineNr       cterm=NONE        ctermfg=08
highlight CursorLineNr cterm=NONE        ctermfg=NONE
highlight Search       ctermbg=03        ctermfg=0
highlight StatusLine   ctermbg=07        ctermfg=08
highlight StatusLineNC ctermbg=NONE      ctermfg=08
highlight VertSplit    ctermbg=NONE      ctermfg=08
highlight Folded       ctermbg=NONE      ctermfg=08
highlight ColorColumn  ctermbg=03        ctermfg=NONE

" 05 is purple. possibly change this
highlight Visual       ctermbg=05
