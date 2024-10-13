;; Sam Sauder
;; 10/11/24
;; Purpose: tags all parts of speech in an input English sentence using a provided lexicon

#lang racket

(require "parser.rkt")
(provide tag tok tags_of)


;; ARGS: a word, a lexicon
;; PRECONDITION: w is in lex
;; Return the tag of w in lex
(define (tag w lex)
    (cond
      [(null? lex) "X"]
      [(equal? w (caar lex))  ;; does w match the first word in the lexicon 
        (cadr (car lex))]    ;; match, return the tag of w in the lexicon 
      
      [else 
        (tag w (cdr lex))]  ;; mismatch, search the rest of the lexicon for a match
      )
    )


;; tokenizes the sentence into a list of words
(define (tok sentence)
    (regexp-split #rx" " sentence))


;; tags
(define (tags_of wlist lex)
    (define tag1 (tag (car wlist) lex))
    (define l (length wlist))
    
    (cond 
        [(= l 1) tag1]  ;; if the length of wlist is 1:
        [else           ;; else the length of wlist > 1:
             (string-append tag1 (tags_of (cdr wlist) lex))]
      )
)

