; Sam Sauder 9/30/24
; Purpose: implementing a basic English sentence validity checker using a 
;   context-free grammar
;   
;   G = (V, Σ, R, S)
;       V = {S, NP, VP, PP, N, V, Det, Adj, Adv}
;       Σ = {a, b, c, ... , x, y, z}
;       S = S

#lang racket


;; ARGS: a phrase, a lexicon
;; Return the phrase type of the given list of words, or
;; 'Z' if the words do not form a phrase
(define (type_of wlist lex)
    (define first_tag (tag (car wlist) lex))
    (cond
        ;;  NP -> N
        [(and (equal? first_tag "N") (equal? (length wlist) 1))  "NP"] 

        ;;  NP -> Det N 
        [(and (equal? first_tag "Det") (equal? (tag (cadr wlist) lex) "N"))  "NP"]


        ;;  VP -> V
        [(and (equal? first_tag "V") (equal? (length wlist) 1))  "VP"] 
        
        ;;  VP -> V NP 
        [(and (equal? first_tag "V") (equal? (type_of (cdr wlist) lex) "NP"))  "VP"]


        ;;  PP -> P NP
        [(and (equal? first_tag "P") (equal? (type_of (cdr wlist) lex) "NP"))  "PP"]


        [else "Z"]
      )
  )


;; ARGS: a word, a lexicon
;; PRECONDITION: w is in lex
;; Return the tag of w in lex
(define (tag w lex)
   (if (equal? w (caar lex))  ;; does w match the first word in the lexicon?
        (cadr (car lex))      ;; match, return the tag of w in the lexicon 
        (tag w (cdr lex))))   ;; mismatch, search the rest of the lexicon for a match 


; Checks a word for syntactic validity 
; a word is valid if its expected tag matches its actual tag (according to the provided lexicon)
;   return #t if 'word' is valid 
;   return #f if 'word' is invalid (syntactically inappropriate)
(define (valid? word tag_expected lex)
    (equal? (tag word lex) tag_expected))


; Program Testing
; ===============

; Test lexicon 
(define l (list 
            [list "see" "V"]
            [list "the" "Det"] 
            [list "thing" "N"]
            [list "over" "P"]
            [list "hedge" "N"]
            ))

;; Test phrase using command line arguments
(define phr
  (vector->list (current-command-line-arguments)))

(type_of phr l)  ;; return the phrase type

