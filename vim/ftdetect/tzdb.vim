fun! s:DetectTZDB()
	if getline(1)[0:5] == '# tzdb'
		set ft=tzdb
	endif
endfun

autocmd BufNewFile,BufRead * call s:DetectTZDB()
