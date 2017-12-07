if exists("g:drawit_mode")
	silent DIstart
	"call SetDrawIt('A','B','C','D','E','F','G')
	call SetDrawIt("│","─","┼","╲","╱","╳","*")
	" setting these also somethow sets the 'half-cross' pieces...?
	" ─│┼╱╳╲
	" down, side intersect right-down left-down intersect other
	silent DIstop
endif
