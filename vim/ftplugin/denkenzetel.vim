function! denkenzetel#toggle()

	let l:pos = getpos(".")

	let l:line = getline(".")
	if l:line == ""
		return
	endif

	if line[0] == '-'
		let l:line = '+' . l:line[1:]
	else
		let l:line = '-' . l:line[1:]
	endif

	call setline(".", l:line)
	call setpos(".", l:pos)

endfunction

command! DenkenzetelToggleItem call denkenzetel#toggle()
nnoremap <C-Space> :DenkenzetelToggleItem<CR>
" C-@ is C-space in terminal
nnoremap <C-@> :DenkenzetelToggleItem<CR>

set nospell
