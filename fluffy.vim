" Vim syntax file
" Language:	fluffy
"

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword fluffyType       byte short int long float double void
syn keyword fluffyType       unsigned signed bool

syn keyword fluffyStatement	goto return continue break step
syn keyword fluffyStatement instance
syn keyword fluffyStatement export enum class
syn keyword fluffyAttribute	extern
syn keyword fluffyRepeat	    for while loop
syn keyword fluffyConditional	if else

syn keyword fluffyConstant    null true false

syn keyword fluffyStatement   struct nextgroup=fluffyIdentifier
syn keyword fluffyStatement   union nextgroup=fluffyIdentifier
syn keyword fluffyStatement   func nextgroup=fluffyIdentifier
syn keyword fluffyStatement   var nextgroup=fluffyIdentifier
syn keyword fluffyStatement   const nextgroup=fluffyIdentifier
syn keyword fluffyStatement   concept nextgroup=fluffyIdentifier
syn keyword fluffyStatement   typealias nextgroup=fluffyIdentifier
syn match   fluffyIdentifier	 "[a-zA-Z_][a-zA-Z0-9_]*" contained

syn keyword fluffyOperator   cast sizeof

syn match   fluffyComment	+//.*$+ contains=fluffyTodo,fluffyComment
syn region  fluffyComment    start=+/\*+ end=+\*/+ contains=fluffyTodo
syn keyword fluffyTodo		TODO FIXME XXX contained

" strings
syn region fluffyString		start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=fluffyEscape
syn region fluffyCharacter   start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=fluffyEscape
syn match  fluffyEscape		+\\[abfnrtv'"\\]+ contained
syn match  fluffyEscape		"\\\o\{1,3}" contained
syn match  fluffyEscape		"\\x\x\{2}" contained

syn match  fluffyNumber	"\<0x\x\+[Ll]\=\>"
syn match  fluffyNumber	"\<\d\+[LljJ]\=\>"
syn match  fluffyNumber	"\.\d\+\([eE][+-]\=\d\+\)\=[jJ]\=\>"
syn match  fluffyNumber	"\<\d\+\.\([eE][+-]\=\d\+\)\=[jJ]\=\>"
syn match  fluffyNumber	"\<\d\+\.\d\+\([eE][+-]\=\d\+\)\=[jJ]\=\>"

syn sync match fluffySync grouphere NONE "):$"
syn sync maxlines=200
"syn sync minlines=2000

hi def link fluffyStatement      Statement
hi def link fluffyConditional    Conditional
hi def link fluffyConstant       Constant
hi def link fluffyString         String
hi def link fluffyCharacter      Character
hi def link fluffyOperator       Operator
hi def link fluffyEscape         Special
hi def link fluffyComment        Comment
hi def link fluffyTodo           Todo
hi def link fluffyNumber         Number
hi def link fluffyRepeat         Repeat
hi def link fluffyCondition      Conditional
hi def link fluffyType           Type
hi def link fluffyAttribute      StorageClass
hi def link fluffyIdentifier     Identifier

let b:current_syntax = "fluffy"

