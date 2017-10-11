" like the `system' command, but only returns the
" first line, without a newline at the end.
"     Should possibly be somewhere else, but it's
" here I use it.
" If nothing was returned from the command return an empty string
function Sys(arg)
	let l:ret = systemlist(a:arg)
	if (empty(l:ret))
		return ""
	else
		return systemlist(a:arg)[0]
	endif
endfunction

let s:cpp  = "-std=gnu++11 "
let s:cpp .= "-I../lib "
let s:cpp .= "-I../lib/StanfordCPPLib "
let s:cpp .= "-fpermissive "
let s:cpp .= Sys("pkg-config --cflags Qt5Sql") . " "
let s:cpp .= Sys("pkg-config --cflags guile-2.2")
let g:syntastic_cpp_compiler_options = s:cpp

let s:c  = "-std=c99 -pedantic "
let s:c .= "-D_XOPEN_SOURCE=600 "
let s:c .= "-lm "
let s:c .= "-I../tetris/src "
"let s:c .= Sys("pkg-config --cflags libpng") . " "
let s:c .= Sys("pkg-config --cflags guile-2.2") . " "
let s:c .= Sys("pkg-config --cflags dbus-1")
let g:syntastic_c_compiler_options = s:c

"let g:syntastic_auto_loc_list = 1
