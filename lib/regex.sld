;;; -*- coding: utf-8; mode: scheme -*-
;;; Eduardo Acuña Yeomans - 2015

(define-library (regex)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme write)
	  (variants)
	  (sets charset)
	  (streams))
  (export
   regex?
   
   regex-nullable
   regex-nullable?

   regex-derive

   rx
   regex-match
   regex-match-string
   regex-match-list
   regex-match-stream

   regex->list

   rx:ascii        rx:any
   rx:nonl
   rx:lower-case   rx:lower
   rx:upper-case   rx:upper
   rx:alphabetic   rx:alpha
   rx:numeric      rx:num
   rx:alphanumeric rx:alphanum rx:alnum
   rx:punctuation  rx:punct
   rx:symbol
   rx:graphic      rx:graph
   rx:printing     rx:print
   rx:whitespace   rx:white    rx:space
   rx:control      rx:cntrl
   rx:hex-digit    rx:xdigit)
  (include "./regex/base.scm"))
