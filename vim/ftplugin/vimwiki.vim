" autocmd BufWritePost *.wiki execute '! cd %:h && git add %:t && git commit -m "% - auto"'

setlocal formatoptions+=t
setlocal expandtab
setlocal shiftwidth=4

setlocal nowrap

nmap <silent><buffer> <CR> <Plug>VimwikiFollowLink
vmap <silent><buffer> <CR> <Plug>VimwikiNormalizeLinkVisualCR
inoremap <silent><buffer> <CR> <Esc>:VimwikiReturn 1 5<CR>

autocmd BufWritePre index.wiki :VimwikiGenerateTagLinks

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
	if link =~# '^\(help\|mail\|man\|info\|local\):'
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
		elseif scheme =~# "man"
			let parts = split(data, "#")
			echo parts
			if len(parts) >= 2
				let [name, section] = parts
				exe printf("!man %s %s", section, name)
			else
				let name = data
				exe printf("!man %s", name)
			endif
			return 1
		elseif scheme =~# "info"
			let parts = split(data, "#")
			if len(parts) >= 2
				let [file, node] = parts
				exe printf("!info %s -n '%s'", file, node)
			else
				exe printf("!info %s", parts[0])
			endif
			return 1
		elseif scheme =~# "mail"
			let mailfile = system("mu find -u 'i:" . matchstr(data, "[^#]*") . "' -f l")
			let cmd = "new +read!mu\\ view\\ " . shellescape(trim(mailfile))
			exe cmd
			exe "setlocal buftype=nofile"
			exe "setf mail"
			exe "setlocal nonu"
			exe "setlocal nospell"
			exe "setlocal readonly"
			exe "goto 1"
			exe "delete"
			" exe "sp " . mailfile
			return 1
		elseif scheme =~# "local"
			exe '!xdg-open "' . expand("%:p:h") . '/' . matchstr(data, "[^#]*") . '" 2>/dev/null &'
			return 1
		else
			return 0
		endif
	endif
endfunction

" TODO help links should also work in HTML output
