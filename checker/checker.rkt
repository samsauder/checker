; Sam Sauder 9/30/24
; Purpose: implementing a basic English sentence validity checker using a 
;   context-free grammar
;   
;   G = (V, Σ, R, S)
;       V = {S, NP, VP, PP, N, V, Det, Adj, Adv}
;       Σ = {a, b, c, ... , x, y, z}
;       S = S

#lang racket

#|
; checks a phrase for validity (a phrase is any element of V for grammar G)
(define (check_one phrase)
  ; which phrase?
  ;     case NP:
  ;         (car phrase) is N OR Det?
  ;             case N:
  ;                 (N (car phrase))
  ;
  ;             case Det:
  ;                 (cadr phrase) is N or Adj?
  ;                  case N: 
  ;                  case Adj:
  ;                     (caddr phrase) is N?
  ;                         yes:
  ;     case VP:
  ;         (car phrase) is V?
  ;             yes: 
  ;                 (cadr phrase) is NP or Adv?
  ;                     case NP:
  ;                         (caddr phrase) is PP?
  ;                             yes: 
  ;                             no: 
  ;                     case Adv:
  ;
  ;     case PP: 
  ;         (car phrase) is P?
  ;             yes: 
  ;                 (cadr phrase) is NP?
  ;                     yes: 
  )
|#


; returns the tag of the word in lexicon or "X" if not present
(define (tag_of word lex)
    (if (equal? '() lex) "X"  ; if lex is empty, return "X"
        (if (equal? (caar lex) word) 
            (cadr (car lex))        ; return the tag of the first word-tag pair (base case)
            (tag_of word (cdr lex))  ; else search the rest of lex (recursive case)
        )
    )
  )

; checks a word for syntactic validity 
; a word is valid if its expected tag matches its actual tag (according to the provided lexicon)
;   return #t if 'word' is valid 
;   return #f if 'word' is invalid (syntactically inappropriate)
(define (valid? word tag_expected lex)
    (equal? (tag_of word lex) tag_expected))


; Program testing
; ===============

; Test lexicon 
(define l (list 
            (list "I" "N") 
            (list "see" "V") 
            (list "something" "N")))

(valid? "see" "V" l)
;; (tag_of "something" l)

#|
; A sentence takes a string
(define (S str)
  ; concatenate NP and VP 
  )

(define (NP str)
  ; call N OR
  ; concatenate Det and N OR
  ; concatenate Det, Adj and N
  )

(define (VP str)
  ; call V OR
  ; concatenate V and NP OR
  ; concatenate V and NP and PP
  ; concatenate V and Adv
  )

(define (PP str)
  ; concatenate P and NP
  )

|#
