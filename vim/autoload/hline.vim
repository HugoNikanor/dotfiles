" Quick script which introduces a higlighted line which can be moved
" up and down with <F2> and <F3>.
" Can't go out of bounds, but can leave the visible part of the
" screen.
" Line additions and deletions also make it jump around quite a bit.

" The plan is to extend this into a frontend for a debugger.

function! hline#setup()
	sign define wholeline linehl=ErrorMsg
	let g:lnr = 1
	let l:fname = expand("%:p")
	" sign place 1 name=wholeline line=l:lnr file=l:fname
	execute printf(":sign place 1 line=%d name=wholeline file=%s",
				\ g:lnr,
				\ l:fname)
endfunction

function! hline#next(step)
	sign unplace 1
	let g:lnr = g:lnr + a:step

	if g:lnr < 1
		let g:lnr = 1
	endif

	if g:lnr > line("$")
		let g:lnr = line("$")
	endif


	let l:fname = expand("%:p")
	execute printf(":sign place 1 line=%d name=wholeline file=%s",
				\ g:lnr,
				\ l:fname)
endfunction

nnoremap <silent> <F2> :call hline#next(-1)<cr>
nnoremap <silent> <F3> :call hline#next(1)<cr>
