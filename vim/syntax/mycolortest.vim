"syntax region Comment start=/#/ end="$" 
syntax match Comment "^#.*" contains=00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15

syntax include @inlineIRC syntax/irc.vim
syntax include @inlineHTML syntax/html.vim

syntax region inlineIRC start="<irc>" keepend end="</irc>"me=s-1 contains=@inlineIRC
syntax region inlineHTML start="<html>" keepend end="</html>"me=s-1 contains=@inlineHTML

syntax match 00 "\(00\s*\)\@<=\S*"  
syntax match 01 "\(01\s*\)\@<=\S*"  
syntax match 02 "\(02\s*\)\@<=\S*"  
syntax match 03 "\(03\s*\)\@<=\S*"  
syntax match 04 "\(04\s*\)\@<=\S*"  
syntax match 05 "\(05\s*\)\@<=\S*"  
syntax match 06 "\(06\s*\)\@<=\S*"  
syntax match 07 "\(07\s*\)\@<=\S*"  
syntax match 08 "\(08\s*\)\@<=\S*"  
syntax match 09 "\(09\s*\)\@<=\S*"  
syntax match 10 "\(10\s*\)\@<=\S*"  
syntax match 11 "\(11\s*\)\@<=\S*"  
syntax match 12 "\(12\s*\)\@<=\S*"  
syntax match 13 "\(13\s*\)\@<=\S*"  
syntax match 14 "\(14\s*\)\@<=\S*"  
syntax match 15 "\(15\s*\)\@<=\S*"  

highlight 00 ctermfg=00
highlight 01 ctermfg=01
highlight 02 ctermfg=02
highlight 03 ctermfg=03
highlight 04 ctermfg=04
highlight 05 ctermfg=05
highlight 06 ctermfg=06
highlight 07 ctermfg=07
highlight 08 ctermfg=08
highlight 09 ctermfg=09
highlight 10 ctermfg=10
highlight 11 ctermfg=11
highlight 12 ctermfg=12
highlight 13 ctermfg=13
highlight 14 ctermfg=14
highlight 15 ctermfg=15

