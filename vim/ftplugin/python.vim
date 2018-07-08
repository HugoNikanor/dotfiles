setlocal expandtab

let shebang = syntastic#util#parseShebang(bufname("%a"))['exe']
let b:syntastic_python_python_exec =
			\ shebang =~# '\m\<python2'
			\ ? 'python2' : 'python3'
