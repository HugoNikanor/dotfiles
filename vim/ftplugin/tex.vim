" note that plaintex is a different thing. And that an empty .tex file is
" interpreted as a plaintex file by default
setlocal textwidth=80

" Turn on spell for language babel is given. Otherwise turn off spell
setlocal spell
" Get language argument from babel inport,
" Strip newline
let @a = system("grep -oP '(?<=\\\\usepackage\\[)[^\\]]*(?=\\]{babel})' " .  expand('%:p') . "| tr -d '\n'")
if @a ==? "swedish"
	setlocal spell spelllang=sv
elseif @a ==? "english"
	setlocal spell spelllang=en
else
	setlocal nospell
endif
