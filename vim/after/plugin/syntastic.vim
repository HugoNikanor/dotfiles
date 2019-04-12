let s:cpp  = "-std=gnu++11 -pedantic -Wall -Wextra "
let g:syntastic_cpp_compiler_options = s:cpp

" let s:c  = "-std=c99 -pedantic -Wall -lm"
" let g:syntastic_c_compiler_options = s:c
" TODO let make only bulid the exact file wanted, instead of running make all
let g:syntastic_c_checkers = ["make", "gcc"]
