scriptencoding utf-8
set encoding=utf-8


set nocompatible

"""""""""""""""""""""""""""""""
" Vundle / Plugin manager
" https://github.com/VundleVim/Vundle.vim
"""""""""""""""""""""""""""""""
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

	Plugin 'VundleVim/Vundle.vim'
	
	" Tmux & vim integration, somehow
	Plugin 'christoomey/vim-tmux-navigator' 

	" Aligning of text
	Plugin 'godlygeek/tabular'

call vundle#end()
filetype plugin indent on



":set autoindent
:set smartindent
:set mouse=a
:set number

:set tabstop=4
:set shiftwidth=4

"""""""""""""""""""""""""""""
" Sets the tabwith to 2 for html files
	autocmd BufNewFile,BufRead *.html set tabstop=2
	autocmd BufNewFile,BufRead *.html set shiftwidth=2

	autocmd BufNewFile,BufRead *.htm  set tabstop=2
	autocmd BufNewFile,BufRead *.htm  set shiftwidth=2

	autocmd BufNewFile,BufRead *.php  set tabstop=2
	autocmd BufNewFile,BufRead *.php  set shiftwidth=2

	autocmd BufNewFile,BufRead *.css  set tabstop=4
	autocmd BufNewFile,BufRead *.css  set shiftwidth=4


" Color marking when passing line 80
	:call matchadd('ColorColumn', '\%81v', 100)

" Highlighting
	:syntax on
	:colorscheme torte
	:set background=light
	:hi ColorColumn ctermbg=5

	" rgb=38,38,38
	:hi normal ctermbg=235
	:hi LineNr ctermfg=yellow 


:set vb

" Display filename on the next to last line
:set laststatus=2
" Show cursor possition
:set ruler

" Highlight all mactching words during search
":set hlsearch

" Show button presses in lower right portion of the screen
:set showcmd

" Ignore case while searching
:set ic

" Tab shows options, <ctrl-d> is also here
:set wildmenu

" Mainly here for eclim
" filetype plugin on

" New splits in better places
:set splitbelow
:set splitright

" ctrl-letter instead of ctrl-w + letter
" Used when switching between splits
	nnoremap <C-J> <C-W><C-J>
	nnoremap <C-K> <C-W><C-K>
	nnoremap <C-L> <C-W><C-L>
	nnoremap <C-H> <C-W><C-H>

""" Folding
set foldenable
set foldmethod=marker
set foldlevelstart=0


""""""""""""""""""""""""""""""""""""""
" Better highlighting when searching "
""""""""""""""""""""""""""""""""""""""

	" Press space to remove search highlight
	:nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

	:hi Search ctermfg=1 ctermbg=3
	
	" Function supposed to make currently marked search term blink
	" But I couldn't get it to work.
	"
	"function! HLNext (blinktime)
	"	let [bufnum, lnum, col, off] = getpos('.')
	"	let matchlen = strlen(matchstr(strpart(getline('.'),col-1),@/))
	"	let target_pat = '\c\%#\%('.@/.'\)'
	"	let ring = matchadd('WhiteOnRed', target_pat, 101)
	"	redraw
	"	exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
	"	call matchdelete(ring)
	"	redraw
	"endfunction

	":call HLNext (10)

	:set incsearch
	:set hlsearch
"""""""""""""""""""""""""""""""""""""""

" :set list " To show the whitespace characters
	exec "set listchars=tab:\u21e5\u00a0,eol:\u00b6,nbsp:\u2423,trail:~"

