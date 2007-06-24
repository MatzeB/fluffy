" Vim syntax file
" Language:	mlang
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword mlangType       byte short int long float double void
syn keyword mlangType       unsigned signed bool

syn keyword mlangStatement	goto return
syn keyword mlangStatement  instance
syn keyword mlangStatement  namespace
syn keyword mlangAttribute	extern
syn keyword mlangRepeat	        for while
syn keyword mlangConditional	if else

syn keyword mlangStatement   struct nextgroup=mlangIdentifier
syn keyword mlangStatement   union nextgroup=mlangIdentifier
syn keyword mlangStatement   func nextgroup=mlangIdentifier
syn keyword mlangStatement   var nextgroup=mlangIdentifier
syn keyword mlangStatement   const nextgroup=mlangIdentifier
syn keyword mlangStatement   typeclass nextgroup=mlangIdentifier
syn keyword mlangStatement   typealias nextgroup=mlangIdentifier
syn match   mlangIdentifier	 "[a-zA-Z_][a-zA-Z0-9_]*" contained

syn keyword mlangOperator   cast __sizeof __typeof

syn match   mlangComment	+//.*$+ contains=mlangTodo,mlangComment
syn region  mlangComment    start=+/\*+ end=+\*/+ contains=mlangTodo
syn keyword mlangTodo		TODO FIXME XXX contained

" strings
syn region mlangString		start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=mlangEscape
syn region mlangCharacter   start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=mlangEscape
syn match  mlangEscape		+\\[abfnrtv'"\\]+ contained
syn match  mlangEscape		"\\\o\{1,3}" contained
syn match  mlangEscape		"\\x\x\{2}" contained

syn match  mlangNumber	"\<0x\x\+[Ll]\=\>"
syn match  mlangNumber	"\<\d\+[LljJ]\=\>"
syn match  mlangNumber	"\.\d\+\([eE][+-]\=\d\+\)\=[jJ]\=\>"
syn match  mlangNumber	"\<\d\+\.\([eE][+-]\=\d\+\)\=[jJ]\=\>"
syn match  mlangNumber	"\<\d\+\.\d\+\([eE][+-]\=\d\+\)\=[jJ]\=\>"

syn sync match mlangSync grouphere NONE "):$"
syn sync maxlines=200
"syn sync minlines=2000

hi def link mlangStatement      Statement
hi def link mlangConditional    Conditional
hi def link mlangString         String
hi def link mlangCharacter      Character
hi def link mlangEscape         Special
hi def link mlangComment        Comment
hi def link mlangTodo           Todo
hi def link mlangNumber         Number
hi def link mlangRepeat         Repeat
hi def link mlangCondition      Conditional
hi def link mlangType           Type
hi def link mlangAttribute      StorageClass
hi def link mlangIdentifier     Identifier

let b:current_syntax = "mlang"

