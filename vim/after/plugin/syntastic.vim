" like the `system' command, but only returns the
" first line, without a newline at the end.
"     Should possibly be somewhere else, but it's
" here I use it.
function Sys(arg)
	return systemlist(a:arg)[0]
endfunction

let cpp  = "-I/usr/include/qt "
let cpp .= "-I/usr/include/qt/QtCore "
let cpp .= "-I/usr/include/qt/QtSql "
let cpp .= "-fpermissive "
let cpp .= Sys("pkg-config --cflags guile-2.2")
let g:syntastic_cpp_compiler_options = cpp

let c  = "-std=c99 -pedantic "
let c .= Sys("pkg-config --cflags guile-2.2")
let c .= Sys("pkg-config --cflags dbus-1")
let g:syntastic_c_compiler_options = c
