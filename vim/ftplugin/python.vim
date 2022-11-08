setlocal expandtab

let shebang = syntastic#util#parseShebang(bufname("%a"))['exe']
let b:syntastic_python_python_exec =
			\ shebang =~# '\m\<python2'
			\ ? 'python2' : 'python3'
let g:syntastic_auto_loc_list = 0
let g:syntastic_python_checkers =
			\ [ 'mypy'
			\ , 'flake8'
			\ ]
