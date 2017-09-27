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
" Binds all colors help needs to my delek colorscheme defined above

hi def link helpIgnore          delek_Ignore
hi def link helpHyperTextJump   delek_Identifier
hi def link helpBar             delek_Ignore
hi def link helpBacktick        delek_Ignore
hi def link helpStar            delek_Ignore
hi def link helpHyperTextEntry  delek_String
hi def link helpHeadline        delek_Statement
hi def link helpHeader          delek_PreProc
hi def link helpSectionDelim    delek_PreProc
hi def link helpVim             delek_Identifier
hi def link helpCommand         delek_Comment
hi def link helpExample         delek_Comment
hi def link helpOption          delek_Type
hi def link helpNotVi           delek_Special
hi def link helpSpecial         delek_Special
hi def link helpNote            delek_Todo
hi def link helpWarning         delek_Todo
hi def link helpDeprecated      delek_Todo

hi def link helpComment         delek_Comment
hi def link helpConstant        delek_Constant
hi def link helpString          delek_String
hi def link helpCharacter       delek_Character
hi def link helpNumber          delek_Number
hi def link helpBoolean         delek_Boolean
hi def link helpFloat           delek_Float
hi def link helpIdentifier      delek_Identifier
hi def link helpFunction        delek_Function
hi def link helpStatement       delek_Statement
hi def link helpConditional     delek_Conditional
hi def link helpRepeat          delek_Repeat
hi def link helpLabel           delek_Label
hi def link helpOperator        delek_Operator
hi def link helpKeyword         delek_Keyword
hi def link helpException       delek_Exception
hi def link helpPreProc         delek_PreProc
hi def link helpInclude         delek_Include
hi def link helpDefine          delek_Define
hi def link helpMacro           delek_Macro
hi def link helpPreCondit       delek_PreCondit
hi def link helpType            delek_Type
hi def link helpStorageClass    delek_StorageClass
hi def link helpStructure       delek_Structure
hi def link helpTypedef         delek_Typedef
hi def link helpSpecialChar     delek_SpecialChar
hi def link helpTag             delek_Tag
hi def link helpDelimiter       delek_Delimiter
hi def link helpSpecialComment  delek_SpecialComment
hi def link helpDebug           delek_Debug
hi def link helpUnderlined      delek_Underlined
hi def link helpError           delek_Error
hi def link helpTodo            delek_Todo
hi def link helpURL             delek_String
