" Used by eclim to fix everything
nnoremap <leader>j :JavaCorrect <cr>

setlocal foldmethod=indent
setlocal foldnestmax=2
setlocal foldlevelstart=99
setlocal foldlevel=99

" Inserts the default main into the file, and puts the user
" into insert mode inside the main method.
"     The inserted text should possibly be stored in some directory,
" possibly: ~/.vim/ftplugin/java.d/Main.java
function! JavaMain()
	:append
class Main {
	public static void main(String[] args) {
		
	}
}
.
normal "kkk"
startinsert!
endfunction

cnoreabbrev JavaMain call JavaMain()
nnoremap <leader>a :call JavaMain()<cr>
