" YYYY-MM-DD HH:mm:SS	[name]	message

highlight Name ctermfg=09
highlight Year ctermfg=10
highlight Time ctermfg=11

highlight Quit ctermfg=01
highlight Join ctermfg=02

highlight Info ctermfg=09
highlight Help ctermfg=14

syntax match Year "\d\{4\}-\d\d-\d\d"
syntax match Time "\d\d:\d\d:\d\d"
syntax match Name "\t.*\t"ms=s+1,me=e-1
syntax match Comment "#.*$"

syntax match Quit "\t<--.*"hs=s+5
syntax match Join "\t-->.*"hs=s+5
syntax match Error "\t=!=.*"hs=s+5
syntax match Info "\t--\t.*"hs=s+4

syntax match Help "\t\t.*"hs=s+2
