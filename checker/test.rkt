;; Sam Sauder
;; 10/13/24
;; Purpose: testing the functions of parser.rkt and tagger.rkt
#lang racket

(require "parser.rkt")
(require "tagger.rkt")


;; USAGE: $ racket test.rkt [sentence] 
;; Returns #t if sentence is valid, #f otherwise

;; test lexicon
(define lex (list
                [list "I" "n"]
                [list "see" "v"]
                [list "the" "d"] 
                [list "thing" "n"]
                [list "that" "d"]
                [list "eat" "v"]
                [list "this" "d"]
                [list "basil" "n"]
            )) 

;; test sentence (the first command line argument)
(define sentence 
    (car (vector->list (current-command-line-arguments))))

;; tokenized sentence
(define wlist (tok sentence))

;; string of tags for every word in the sentence
(define tags_str (tags_of wlist lex))

tags_str
(S tags_str)  ;; is the sentence grammatically valid?

