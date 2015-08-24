" Vim syntax file
" Language:    NTCC
" Maintainer:  Jaime Arias <jaime.arias@labri.fr>
" Last Change: Sept 6 2014
" Version:     1


if exists("b:current_syntax")
  finish
endif

" No case sensitive
syn case ignore


" Comments
syn keyword ntccTodo     contained TODO FIXME XXX NOTE
syn region  ntccComment    start="/\*\*" end="\*\*/" contains=ntccTodo
syn match  ntccCommentLine "//.*$" contains=ntccTodo


syn keyword ntccProcessKeyword Tell Next Unless Choice When
syn keyword ntccBool true false

syn match ntccConstraint "\([a-z][0-9a-z]*\|\^\)"

syn match ntccOperator ":" "+"


" || * !

hi def link ntccComment Comment
hi def link ntccCommentLine Comment

hi def link ntccProcessKeyword Constant

hi def link ntccConstraint Special
hi def link ntccBool  Todo

hi def link ntccOperator Operator

let b:current_syntax = "ntcc"

