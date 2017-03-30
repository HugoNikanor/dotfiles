" Used by eclim to fix everything
nnoremap <leader>j :JavaCorrect <cr>

setlocal foldmethod=indent
setlocal foldnestmax=2
setlocal foldlevelstart=99
setlocal foldlevel=99

function! JavaMain()
	r ~/Templates/java/main
endfunction
cnoreabbrev JavaMain call JavaMain()
