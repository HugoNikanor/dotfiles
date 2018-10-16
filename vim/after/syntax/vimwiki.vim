if v:version > 800 || v:version == 800 && has("patch1038")
	highlight VimwikiDelText term=strikethrough cterm=strikethrough gui=strikethrough
endif
