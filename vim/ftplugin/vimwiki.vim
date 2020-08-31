setlocal formatoptions+=t
setlocal expandtab
setlocal shiftwidth=2

nmap <silent><buffer> <CR> <Plug>VimwikiFollowLink
vmap <silent><buffer> <CR> <Plug>VimwikiNormalizeLinkVisualCR
inoremap <silent><buffer> <CR> <Esc>:VimwikiReturn 1 5<CR>

autocmd BufWritePre index.wiki :VimwikiGenerateTags

let g:tagbar_type_vimwiki = {
			\   'ctagstype':'vimwiki'
			\ , 'kinds':['h:header']
			\ , 'sro':'&&&'
			\ , 'kind2scope':{'h':'header'}
			\ , 'sort':0
			\ , 'ctagsbin':'~/vimwiki/vimwiki-utils/vwtags.py'
			\ , 'ctagsargs': 'default'
			\ }

" !xdg-open ...
" echomsg link_infos.index " 0
" echomsg link_infos.scheme " wiki0
" echomsg link_infos.filename " /home/...
" echomsg link_infos.anchor " [empty?]
function! VimwikiLinkHandler(link)
	let link = a:link
	if link =~# '^\(help\|mail\):'
		let [scheme, data] = split(link, ":")
	else " Default handler
		return 0
	endif

	let link_infos = vimwiki#base#resolve_link(link)
	if link_infos.filename == ''
		echomsg 'Vimwiki Error: Unable to resolve link!'
		return 0
	else
		if scheme =~# "help"
			exe "help " .  matchstr(data, "[^/]*$")
			return 1
		elseif scheme =~# "mail"
			let mailfile = system("mu find -u 'i:" . matchstr(data, "[^#]*") . "' -f l")
			exe "sp " . mailfile
		else
			return 0
		endif
	endif
endfunction

" TODO help links should also work in HTML output
