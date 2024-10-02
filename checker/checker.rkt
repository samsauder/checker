; Sam Sauder 9/30/24
; Purpose: implementing a basic English sentence validity checker using a 
;   context-free grammar
;   
;   G = (V, Σ, R, S)
;       V = {S, NP, VP, PP, N, V, Det, Adj, Adv}
;       Σ = {a, b, c, ... , x, y, z}
;       S = S

#lang racket


; returns phrase_type if the given phrase is composed of a sequence
;   of words that are in accordance to one of the valid sequences for 
;   its given phrase type, 'Z' otherwise 
; args: phrase, phrase_type
(define (seq_verify phrase lex)
    ; make a list of the phrase's associated tags
    (define taglist 
        (map (lambda (p)
                (tag p lex))  
             phrase))  ;; testing new tag function
    
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


; return the tag of the given phrase (single word or list of words)
(define (tag phrase lex)
    (cond
        [(equal? '() lex) "M"]  ;; lexicon has been fully searched

        [(= (length phrase) 1)  ;; phrase is a single word  
            (if (equal? (caar lex) phrase)
                    (cadr (car lex))  ; return the tag of the first word
                    (tag phrase (cdr lex))  ; else
              )
         ]

        [(> (length phrase) 1)  ;; phrase is a list of words
            (seq_verify phrase lex) 
         ]
      )
  )


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
            (list "I" "N") 
            (list "see" "V") 
            (list "something" "N")))

(seq_verify (list "see" "something") l)

;; (valid? "see" "V" l)
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
