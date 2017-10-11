set nospell
set nonumber

" Colorscheme deffinition {{{1
" This is the delek colorscheme, but with `delek_` prefixed to everything

" Normal should come first
hi delek_Normal     guifg=Black  guibg=White
hi delek_Cursor     guifg=bg     guibg=fg
hi delek_lCursor    guifg=NONE   guibg=Cyan

" Note: we never set 'term' because the defaults for B&W terminals are OK
hi delek_DiffAdd    ctermbg=LightBlue    guibg=LightBlue
hi delek_DiffChange ctermbg=LightMagenta guibg=LightMagenta
hi delek_DiffDelete ctermfg=Blue         ctermbg=LightCyan gui=bold guifg=Blue guibg=LightCyan
hi delek_DiffText   ctermbg=Red          cterm=bold gui=bold guibg=Red
hi delek_Directory  ctermfg=DarkBlue     guifg=Blue
hi delek_ErrorMsg   ctermfg=White        ctermbg=DarkRed  guibg=Red       guifg=White
hi delek_FoldColumn ctermfg=DarkBlue     ctermbg=Grey     guibg=Grey      guifg=DarkBlue
hi delek_Folded     ctermbg=Grey         ctermfg=DarkBlue guibg=LightGrey guifg=DarkBlue
hi delek_IncSearch  cterm=reverse        gui=reverse
hi delek_LineNr     ctermfg=Brown        guifg=Brown
hi delek_ModeMsg    cterm=bold           gui=bold
hi delek_MoreMsg    ctermfg=DarkGreen    gui=bold guifg=SeaGreen
hi delek_NonText    ctermfg=Blue         gui=bold guifg=gray guibg=white
hi delek_Pmenu      guibg=LightBlue
hi delek_PmenuSel   ctermfg=White        ctermbg=DarkBlue  guifg=White  guibg=DarkBlue
hi delek_Question   ctermfg=DarkGreen    gui=bold guifg=SeaGreen
if &background == "light"
    hi delek_Search     ctermfg=NONE     ctermbg=Yellow guibg=Yellow guifg=NONE
else
    hi delek_Search     ctermfg=Black    ctermbg=Yellow guibg=Yellow guifg=Black
endif
hi delek_SpecialKey ctermfg=DarkBlue     guifg=Blue
hi delek_StatusLine cterm=bold           ctermbg=blue ctermfg=yellow guibg=gold guifg=blue
hi delek_StatusLineNC cterm=bold         ctermbg=blue ctermfg=black  guibg=gold guifg=blue
hi delek_Title      ctermfg=DarkMagenta  gui=bold guifg=Magenta
hi delek_VertSplit  cterm=reverse        gui=reverse
hi delek_Visual     ctermbg=NONE         cterm=reverse gui=reverse guifg=Grey guibg=fg
hi delek_VisualNOS  cterm=underline,bold gui=underline,bold
hi delek_WarningMsg ctermfg=DarkRed      guifg=Red
hi delek_WildMenu   ctermfg=Black        ctermbg=Yellow    guibg=Yellow guifg=Black

" syntax highlighting
hi delek_Comment    cterm=NONE ctermfg=DarkRed     gui=NONE guifg=red2
hi delek_Constant   cterm=NONE ctermfg=DarkGreen   gui=NONE guifg=green3
hi delek_Identifier cterm=NONE ctermfg=DarkCyan    gui=NONE guifg=cyan4
hi delek_PreProc    cterm=NONE ctermfg=DarkMagenta gui=NONE guifg=magenta3
hi delek_Special    cterm=NONE ctermfg=LightRed    gui=NONE guifg=deeppink
hi delek_Statement  cterm=bold ctermfg=Blue        gui=bold guifg=blue
hi delek_Type       cterm=NONE ctermfg=Blue        gui=bold guifg=blue

" Colorscheme binding {{{1
" This maps help highlighting to `delek_' where that's available
" And to the current colorscheme where `delek_' doesn't export any
" highlighting.
"     Note that my `Comments' colorscheme doesn't set all these colors
" to something non-white. But it at least sets `String' and `Keyword'.

"hi def link helpIgnore          Ignore
hi def link helpHyperTextJump   delek_Identifier
"hi def link helpBar             Ignore
"hi def link helpBacktick        Ignore
"hi def link helpStar            Ignore
"hi def link helpHyperTextEntry  String
hi def link helpHeadline        delek_Statement
hi def link helpHeader          delek_PreProc
hi def link helpSectionDelim    delek_PreProc
hi def link helpVim             delek_Identifier
hi def link helpCommand         delek_Comment
hi def link helpExample         delek_Comment
hi def link helpOption          delek_Type
hi def link helpNotVi           delek_Special
hi def link helpSpecial         delek_Special
"hi def link helpNote            Todo
"hi def link helpWarning         Todo
"hi def link helpDeprecated      Todo

"hi def link helpComment         Comment
hi def link helpConstant        delek_Constant
"hi def link helpString          String
"hi def link helpCharacter       Character
"hi def link helpNumber          Number
"hi def link helpBoolean         Boolean
"hi def link helpFloat           Float
hi def link helpIdentifier      delek_Identifier
"hi def link helpFunction        Function
hi def link helpStatement       delek_Statement
"hi def link helpConditional     Conditional
"hi def link helpRepeat          Repeat
"hi def link helpLabel           Label
"hi def link helpOperator        Operator
"hi def link helpKeyword         Keyword
"hi def link helpException       Exception
hi def link helpPreProc         delek_PreProc
"hi def link helpInclude         Include
"hi def link helpDefine          Define
"hi def link helpMacro           Macro
"hi def link helpPreCondit       PreCondit
hi def link helpType            delek_Type
"hi def link helpStorageClass    StorageClass
"hi def link helpStructure       Structure
"hi def link helpTypedef         Typedef
"hi def link helpSpecialChar     SpecialChar
"hi def link helpTag             Tag
"hi def link helpDelimiter       Delimiter
"hi def link helpSpecialComment  SpecialComment
"hi def link helpDebug           Debug
"hi def link helpUnderlined      Underlined
"hi def link helpError           Error
"hi def link helpTodo            Todo
"hi def link helpURL             String
