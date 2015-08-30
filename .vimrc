":set autoindent
:set smartindent
:set mouse=nicr
:set number

:set tabstop=4
:set shiftwidth=4

:color torte
:set background=light

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

" Tab shows options
:set wildmenu

" Mainly here for eclim
filetype plugin on

" New splits in better places
:set splitbelow
:set splitright

" ctrl-letter instead of ctrl-w + letter
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"if filetype == html | php 
