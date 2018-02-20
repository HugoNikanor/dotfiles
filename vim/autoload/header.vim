" Simple function which flips between the
" code and header file.
" TODO make it look for different file endings
" other than .{c,h}, so that it can work with c++
function! header#change()
	let noext = expand("%:r")
	let ext   = expand("%:e")

	if ext == "h"
		let next = "c"
	else
		let next = "h"
	endif

	let ofile = noext . "." . next

	execute 'edit' ofile
endfunction
