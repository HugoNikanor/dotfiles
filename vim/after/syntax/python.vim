syntax region Comment start=/"""/ end=/"""/
syntax region Comment start=/'''/ end=/'''/


" Used for SqlAlchemy query strings.
" TODO I want to enable syntax highlighting of SQL inside these
" strings.
syntax region String start=/text\_s*(\_s*\zs"""/ end=/"""\ze\_s*)/
syntax region String start=/text\_s*(\_s*\zs'''/ end=/'''\ze\_s*)/
