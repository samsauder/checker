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
; returns phrase_type if the given phrase is composed of a sequence
;   of words that are in accordance to one of the valid sequences for 
;   its given phrase type, 'Z' otherwise 
; args: phrase, phrase_type
(define (seq_verify phrase lex)
    (define taglist (tag_all phrase lex))
    
    (display "taglist: ")
    (display taglist)
    (display "\n")

    (cond  ;; what type of phrase? 
        ;; NP
        [(equal? taglist (list "N"))  "NP"]
        [(equal? taglist (list "Det" "N"))  "NP"]
        [(equal? taglist (list "Det" "Adj" "N"))  "NP"]
         
        ;; VP
        [(equal? taglist (list "V"))  "VP"]
        [(equal? taglist (list "V" "NP"))  "VP"]
        [(equal? taglist (list "V" "Adv"))  "VP"]

        ;; PP
        [(equal? taglist (list "P" "NP"))  "PP"]

        [else  "Z"])
    )
|#


;; type_of
;; ==================================================================
;;      get the tag of the first element of the list
;;      seq_verify rest of list

;;      NP:
;;      if the tag of the first word in the list is "Det" AND
;;      the tag of the second word in the list is "N", THEN
;;          return "NP"

;;      VP:
;;      if the tag of the first word in the list is "V" AND 
;;      the phrase type of the rest of the list is "NP", THEN
;;          return "VP"

;;      PP:
;;      if the tag of the first word in the list is "P" AND
;;      the phrase type of the rest of the list is "NP", THEN 
;;          return "PP"
;; 


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

#|
;; tag_all
;; Args: a list of words
;; Tags the first word in the list and appends that value with the tag of the rest of the list
(define (tag_all phrase lex)
    (map 
      (lambda (p)
        (tag p lex))     
      phrase)
    )
|#

; return the tag of the given phrase (single word or list of words)
(define (tag phrase lex)
    (cond
        [(not (list? phrase))  ;; phrase is a single word
            (cond
                [(equal? (caar lex) phrase)  ;; 'phrase' matches the first word in the lexicon
                    (cadr (car lex))]        ;; return the tag of 'phrase'

                [else 
                  (tag phrase (cdr lex))]  ;; tag 'phrase' using the rest of the lexicon
            )]    
       
        [(list? phrase)  ;; phrase is a list of words
            (type_of phrase lex)  ;; testing new function
           ]
        ))


; checks a word for syntactic validity 
; a word is valid if its expected tag matches its actual tag (according to the provided lexicon)
;   return #t if 'word' is valid 
;   return #f if 'word' is invalid (syntactically inappropriate)
(define (valid? word tag_expected lex)
    (equal? (tag word lex) tag_expected))


; Program testing
; ===============

; Test lexicon 
(define l (list 
            [list "see" "V"]
            [list "the" "Det"] 
            [list "thing" "N"]
            [list "over" "P"]
            [list "hedge" "N"]
            ))


(type_of (list "see" "the" "thing") l)

