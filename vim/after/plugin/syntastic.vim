let g:syntastic_cpp_compiler_options = " -I/usr/include/qt -isystem /usr/include/qt/QtCore -isystem /usr/include/qt/QtSql -fpermissive " . system("echo -n `pkg-config --cflags guile-2.2`")
