" This file is set up for sqlite usage
"
" I'm also not certain that everything below is correctly grouped
syntax case ignore

syntax keyword sqlType text timestamp glob

syntax keyword sqlKeyword pragma limit temp temporary without virtual vacuum

syntax keyword sqlStatement primary foreign references autoincrement notnull
syntax keyword sqlStatement before after instead of each key

syntax keyword sqlFunction ifnull datetime current_time isnull

" syntax [] sqlFold
