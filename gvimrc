" Set's up GVim as a small floating terminal,
" The floating part should be handled by the window manager 

set lines=24 columns=80
" disables all silly gui stuff
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

set nonumber
set laststatus=0

set novisualbell

set noruler
set noshowcmd
set statusline=""

colorscheme darkblue

" Make the Gui Window semi transparent.
" This should preferably be changed so it's set in the wm settings
autocmd GUIEnter    * silent exec "!transset-df -a 0.9"
"autocmd FocusLost   * exec "!transset-df -n "GVIM$" 0.5"
"autocmd FocusGained * exec "!transset-df -a 0.9"
