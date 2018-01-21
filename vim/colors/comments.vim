" https://gist.github.com/hkmix/41492855c3fcc7a9393b
"
" I have also modified this layout a bit.
" Currently comments and docstrings should be grey,
" while literals should be green.
"
" TODO make it easy to change colour of groups.

set background=dark
highlight clear
if exists("syntax_on")
    syntax reset
endif

let colors_name = "comments"

" TODO something for ssh sessions not using tmux
if $TERM ==? "linux"
	highlight Visual ctermbg=NONE cterm=reverse gui=reverse guifg=Grey guibg=fg
elseif expand($TMUX) != '' " If $TMUX is set
	highlight Visual ctermbg=NONE cterm=reverse gui=reverse guifg=Grey guibg=fg
else " probably an xterm of some sort
	highlight Comment ctermfg=04
	highlight StatusLine   ctermbg=07        ctermfg=08
	highlight StatusLineNC ctermbg=NONE      ctermfg=08
	highlight Visual       ctermbg = DarkGrey
endif

" First set Normal to regular white on black text colors:
highlight Normal ctermfg=NONE ctermbg=NONE guifg=#dddddd guibg=#505050

" Syntax highlighting (other color-groups using default, see :help group-name):
highlight Comment      cterm=NONE        ctermfg=08
highlight String       cterm=NONE        ctermfg=02
" This still shows up as 02
highlight Keyword      cterm=NONE        ctermfg=02
highlight Constant     cterm=NONE        ctermfg=07
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

highlight String       cterm=NONE        ctermfg=02
highlight Character    cterm=NONE        ctermfg=02
highlight Number       cterm=NONE        ctermfg=02
highlight Boolean      cterm=NONE        ctermfg=02
highlight Float        cterm=NONE        ctermfg=02

highlight VertSplit    ctermbg=NONE      ctermfg=08
highlight Folded       ctermbg=NONE      ctermfg=08
highlight ColorColumn  ctermbg=03        ctermfg=NONE

"highlight link schemeQuoted String
