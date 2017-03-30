syntax region Comment start=/#|/ end=/|#/
syntax region Comment start=/#!/ end=/!#/ fold

syntax region Comment start=/\((define\S*\s*(.*)\_s*\)\@<="/ skip=/\\"/ end=/"/

syntax match Keyword "\<#:\S\+\>"

" Possibly group this with something other than Function
syntax keyword Function -> ->>
