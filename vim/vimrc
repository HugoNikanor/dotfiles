""" Don't touch stuff {{{
scriptencoding utf-8
set encoding=utf-8

set nocompatible

set t_Co=256

"}}}

" Vundle / Plugin manager {{{
" https://github.com/VundleVim/Vundle.vim

filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

	Plugin 'VundleVim/Vundle.vim'

	" Tmux & vim integration, somehow
	Plugin 'christoomey/vim-tmux-navigator'
	" Aligning of text
	Plugin 'godlygeek/tabular'
	" Draw lines and boxes
	Plugin 'hrj/vim-DrawIt'
	Plugin 'lervag/vimtex'
	Plugin 'scrooloose/nerdtree'
	Plugin 'Xuyuanp/nerdtree-git-plugin'
	Plugin 'tpope/vim-surround'
	Plugin 'tpope/vim-dispatch'
	Plugin 'scrooloose/syntastic'
	Plugin 'ctrlpvim/ctrlp.vim'
	Plugin 'derekwyatt/vim-scala'

call vundle#end()
filetype plugin indent on

"}}}

" Plugin settings {{{
":call SetDrawIt('A','B','C','D','E','F','G')
":call SetDrawIt("-","|","-","â½","/","/","*")
" setting these also somethow sets the 'half-cross' pieces...?
" ─│┼╱╳╲
" down, side intersect right-down left-down intersect other
" }}}

" Tabs {{{
set tabstop=4
set shiftwidth=4

" Sets the tabwith to 2 for html files
	autocmd BufNewFile,BufRead *.html set tabstop=2
	autocmd BufNewFile,BufRead *.html set shiftwidth=2

	autocmd BufNewFile,BufRead *.htm  set tabstop=2
	autocmd BufNewFile,BufRead *.htm  set shiftwidth=2

	autocmd BufNewFile,BufRead *.php  set tabstop=2
	autocmd BufNewFile,BufRead *.php  set shiftwidth=2

	autocmd BufNewFile,BufRead *.css  set tabstop=4
	autocmd BufNewFile,BufRead *.css  set shiftwidth=4

" Soft tabs in haskell
	autocmd BufNewFile,BufRead *.hs set expandtab

	set softtabstop=4
	autocmd Filetype python set expandtab
"}}}

" Higlighting {{{
" Color marking when passing line 80
	call matchadd('ColorColumn', '\%81v', 100)

"" Highlighting
	syntax on
	if $TERM ==? "linux"
		"00 black
		"01 red
		"02 green
		"03 orange
		"04 Blue
		"05 Purple
		"06 Cyan
		"07 White
		colorscheme comments-simple
		hi Search ctermbg=01
		hi Comment ctermfg=03
		hi WildMenu ctermfg=01
		exec "set listchars=tab:>-,eol:%,nbsp:_,trail:~"
	elseif ($TERM ==? "xterm") + ($TERM ==? "xterm-termite")
		colorscheme comments
		exec "set listchars=tab:>—,eol:\u00b6,nbsp:\u2423,trail:~"
	else " This should be targeted at tmux
		"colorscheme preto
		colorscheme comments-simple
		exec "set listchars=tab:>-,eol:\u00b6,nbsp:\u2423,trail:~"
		"syntax off
	endif
	"hi ColorColumn ctermbg=5

	autocmd Filetype lisp call SetLispMode()
	autocmd Filetype scheme call SetLispMode()
	function SetLispMode()
		set expandtab
		set tabstop=2
		set shiftwidth=2
	endfunction


	autocmd Filetype markdown call SetMdMode()
	function SetMdMode()
		"colorscheme myDef
		" highlight double space at the end of line
		syntax match Visual "\s\{2}$"
	endfunction

	autocmd Filetype tex call SetTexMode()
	function SetTexMode()
		set textwidth=80

		" Turn on spell for language babel is given. Otherwise turn off spell
		set spell
		" Get language argument from babel inport,
		" Strip newline
		let @a = system("grep -oP '(?<=\\\\usepackage\\[)[^\\]]*(?=\\]{babel})' " .  expand('%:p') . "| tr -d '\n'")
		if @a ==? "swedish"
			set spell spelllang=sv
		elseif @a ==? "english"
			set spell spelllang=en
		else
			set nospell
		endif
	endfunction

	hi NerdTreeDir ctermfg=blue
	hi NERDtreeExecFile ctermfg=10
	hi NERDtreeRO ctermfg=06
	hi NERDtreeOpenable ctermfg=07
	hi NERDtreeClosable ctermfg=07

	syntax match Error "\s*$"

"}}}

" Key remaps {{{
	""" maps space to <leader>, and makes it show up with 'showcmd'
	"nnoremap <leader> \
	"nmap <space> \

	""nnoremap <space> <leader>
	let mapleader=" "

	""" Moving between buffers
	nnoremap <C-J> <C-W><C-J>
	nnoremap <C-K> <C-W><C-K>
	nnoremap <C-L> <C-W><C-L>
	nnoremap <C-H> <C-W><C-H>

	""" Leader maps
	nnoremap <silent> <leader>o :nohlsearch <cr>
	nnoremap <leader>h :vertical help
	" Used by eclim to fix everything
	nnoremap <leader>j :JavaCorrect <cr>

	nnoremap <leader>t iTODO

	nnoremap <leader>s :%s/\s\+$// <cr>

	nnoremap <leader>d 0D
	nnoremap <silent> <leader>a :set foldlevel=0 <cr>

	""" other
	nnoremap <cr> o<esc>
	" uses what looks like a line, instead of the actual lines
	nnoremap j gj
	nnoremap k gk
	nnoremap gj j
	nnoremap gk k

"}}}

" Folding {{{
	set foldmethod=marker
	set foldnestmax=20
	set foldlevelstart=0
	set foldlevel=0

	autocmd Filetype java call SetJavaIndent()
	function SetJavaIndent()
		set foldmethod=indent
		set foldnestmax=2
		set foldlevelstart=99
		set foldlevel=99
	endfunction

	autocmd Filetype lua call SetLuaIndent()
	function SetLuaIndent()
		set foldmethod=indent
		set foldnestmax=1
		set foldlevelstart=99
		set foldlevel=99
	endfunction

	set foldenable
"}}}

" Functions {{{
	function! HandleURL()
		let uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
		if uri != ""
			" on desktop, if I have 'xdg-open' here then chrome is opened
			" even though elinks is set as the default browser.
			silent exec "!tmux new-window -n \"elinks(vim)\" elinks ".uri
			redraw!
		endif
	endfunction
	map gx :call HandleURL()<cr>

	function! InterpretScheme()
		!car % | scheme
	endfunction

	function! JavaMain()
		r ~/Templates/java/main
	endfunction
	cnoreabbrev JavaMain call JavaMain()

" }}}

" Binary Files {{{
" I'm not actually sure that this work as intended
" When I tried it on a larger wav file it might have corrupted it.
function! BinaryEdit()
	set binary
	set noeol
	%!xxd
endfunction
" the wav can be extended to match any types of binary files
autocmd BufNewFile,BufRead *.wav silent call BinaryEdit()
autocmd BufWritePre *.wav silent %!xxd -r
autocmd BufWritePost *.wav silent %!xxd
" }}}

" ex aliases {{{
	cnoreabbrev ntt NERDTreeToggle
" }}}

" Other {{{

set smartindent
set mouse=a

set number

" makes wrapped lines have the same indention as the original line
" set breakindent

" use a visual cue instead of a sound cue for messages
set visualbell

" Display filename on the next to last line
set laststatus=2

" Show cursor possition in info bar
set ruler

" Show button presses in lower right portion of the screen
set showcmd

" Ignore case while searching
set ignorecase

" Tab shows options, <ctrl-d> is used to show all possabilities
set wildmenu

" New splits in better places
set splitbelow
set splitright


""" Searching
set incsearch
set hlsearch

set completeopt=longest,menuone

" Whitespace highlighting
" ':set list' to enable
"exec "set listchars=tab:>\u2015,eol:\u00b6,nbsp:\u2423,trail:~"

com! FormatJSON %!python -m json.tool

" disable terminal flashing on error
set t_vb=

"}}}

