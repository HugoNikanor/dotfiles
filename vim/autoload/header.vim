" Simple function which flips between the
" code and header file.
function! header#change()
	let noext = expand("%:r")
	let ext   = expand("%:e")

	# Which filetypes can correspond to which types.
	# Make sure that a round trip is always possible
	# (e.g. since .cc points to .h than .h should point to .cc)
	# "h" first in all cases since that's my prefered scheme, weird h
	# variants are bound to their weird c variants.
	let d = {
				\ "h": [ "c", "cpp", "cc" ],
				\ "hpp": [ "cpp" ],
				\ "hh": [ "cc" ],
				\ "c": [ "h" ],
				\ "cc": [ "h", "hh" ],
				\ "cpp": [ "h", "hpp" ],
				\ }

	let options = get(d, ext, [])
	# Do nothing if we aren't a c or h file.
	if empty(options)
		return
	endif

	# Check each possible extension, and open the first mathc
	for extension in options
		let ofile = noext . "." . extension
		if filereadable(ofile)
			execute 'edit' ofile
			return
		endif
	endfor

	# If we didn't match, open a new file with the prefered extension
	let ofile = noext . "."  . options[0]
	execute 'edit' ofile
endfunction
