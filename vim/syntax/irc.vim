" YYYY-MM-DD HH:mm:SS	[name]	message


syntax match Year "\d\{4\}-\d\d-\d\d"
syntax match Time "\d\d:\d\d:\d\d"
syntax match Name "\t.*\t"ms=s+1,me=e-1
syntax match Comment "#.*$"

syntax match Quit "\t<--.*"hs=s+5
syntax match Join "\t-->.*"hs=s+5
syntax match Error "\t=!=.*"hs=s+5
syntax match Info "\t--\t.*"hs=s+4

syntax match Help "\t\t.*"hs=s+2
