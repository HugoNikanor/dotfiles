" note that plaintex is a different thing. And that an empty .tex file is
" interpreted as a plaintex file by default

setlocal textwidth=60

" Turn on spell for language babel is given. Otherwise turn off spell

setlocal spell

" Get language argument from babel import.
let pat = '\\usepackage\[\zs\w\+\ze\]{babel}'
let match = matchstr(getline(search(pat)), pat)
if match ==? "swedish"
	setlocal spell spelllang=sv
elseif match ==? "english"
	setlocal spell spelllang=en
else
	setlocal nospell
endif

set makeprg=latexmk\ -pdf\ %<.tex

" iabbrev ==> \Longrightarrow
