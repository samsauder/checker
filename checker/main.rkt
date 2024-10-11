; Sam Sauder 9/30/24
; Purpose: implementing a basic English sentence validity checker using a 
;   context-free grammar
;   
;   G = (V, Σ, R, S)
;       V = {S, NP, VP, PP, N, V, Det, Adj, Adv}
;       Σ = {a, b, c, ... , x, y, z}
;       S = S
;       R = {...}

#lang racket
(provide type_of type_all tag)

;; ARGS: a list of words, a lexicon
;; Return the (phrase type and last word in wlist) of the given list of words, or
;; 'Z' if the words do not form a phrase
(define (type_of wlist lex)
    (define first_tag (tag (car wlist) lex))
    (displayf "first_tag" first_tag)

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


;; INPUT: a list of words, a lexicon 
;; OUTPUT: #t if wlist is a sentence, #f otherwise 
;; (define (sentence? wlist lex)
;;      ;; if wlist starts with a NP:
;;      ;;      let Q be the sublist of wlist starting after the last word in the NP
;;
;;      ;;      if the type of Q is VP: return #t 
;;      ;;      else: return #f
;;      ;; else: return #f
;; 
;; )
;;

;; =======================================================
;; NEW HELPER FUNCTIONS
;; =======================================================
;; subafter_NP:
;; ARGS: list of words, lexicon
;; returns the sublist immediately following the last word of the first NP of wlist 
;;   otherwise return null


(define (subafter wlist lex)  
    (define tag1 (tag (car wlist) lex))  ;; tag associated with the first word 

    (cond 
        [(equal? tag1 "N") 
            (cdr wlist)]  ;; base case
        [else 
            (subafter (cdr lex) lex)
         ]  ;; rec case
      )    
  )


;; (subafter (list ) )
(exit)




;; =======================================================






#|
;; returns the word following the first found phrase in wlist
;; "none" otherwise
(define (following_word wlist lex)
    (define first_tag (tag (car wlist) lex))
    (displayf "first_tag" first_tag)

    (cond
        ;;  NP -> N
        [(and (equal? first_tag "N")  
              (> (length wlist) 1))
         (cadr wlist)]

        ;;  NP -> Det N 
        [(and (equal? first_tag "Det") 
              (equal? (tag (cadr wlist) lex) "N")
              (> (length wlist) 2))  
         (caddr wlist)]


        ;;  VP -> V
        [(and (equal? first_tag "V")  
              (> (length wlist) 1)) 
         (cadr wlist)] 
        
        ;;  VP -> V NP 
        [(and 
           (equal? first_tag "V") 
           (equal? (type_of (cdr wlist) lex) "NP"))  
         (following_word (cdr wlist) lex)]  ;; find the following word of the NP
         


        ;;  PP -> P NP
        [(and 
           (equal? first_tag "P") 
           (equal? (type_of (cdr wlist) lex) "NP"))  
         (following_word (cdr wlist) lex)]


        [else "None"]
      )
  )
|#






#|
;; Find the types of two phrases in wlist
;; ARGS: a list of words, the first phrase built so far, 
;; , a tuple containing the type of both phrases, a lexicon 
;; Return #t if wlist is a sentence
(define (type_all wlist phr type_tuple lex)
    (define phr_new
      (append phr (list (car wlist)))) 
    
    (define t (type_of phr_new lex))  ;; type of the first candidate phrase so far
    (define type_tuple_new (append type_tuple (list t)))

    (displayf "type_tuple_new" type_tuple_new)
    (displayf "ideal: " (list "NP" "VP"))
    
    (cond 
      [(null? wlist) 
        (if (equal? type_tuple_new (list "NP" "VP"))
            #t  ;; valid sentence 
            #f  ;; invalid sentence
            )]
      ) 

    (displayf "\n\nwlist" wlist)
    (displayf "phr" phr)
    (displayf "phr_new" phr_new)
    (displayf "type_tuple" type_tuple)
    ;; (displayf "lex" lex)
    (displayf "t" t)

    (cond
        [(not (eq? t "Z"))  ;; phrase is valid 
            (type_all (cdr wlist) null (append type_tuple (list t)) lex)] 
        [else               ;; phrase is invalid  
            (type_all (cdr wlist) phr_new type_tuple lex)])
  )
|#

#|
;; ARGS: a list of words, a word to stop after
;; returns a sublist of the given list beginning after the given word in that list
;;  (null if word can't be found)
(define (sub_after wlist word)
    (if (null? wlist) null)

    (if (equal? (car wlist) word) 
        (cdr wlist)  ;; return the sublist after word
        (sub_after (cdr wlist) word)  ;; else 
    ) 
)
|#


;; better formatting display
(define (displayf label str)
    (display label)
    (display ": ")
    (display str)
    (display "\n\n")
  )

;; ARGS: a word, a lexicon
;; PRECONDITION: w is in lex
;; Return the tag of w in lex
(define (tag w lex)
    (cond
      [(null? lex) "X"]
      [(eq? w (caar lex))  ;; does w match the first word in the lexicon 
        (cadr (car lex))]    ;; match, return the tag of w in the lexicon 
      
      [else 
        (tag w (cdr lex))]  ;; mismatch, search the rest of the lexicon for a match
      )
    )


; Program Testing
; ===============

; Test lexicon 
(define l (list
            [list "I" "N"]
            [list "see" "V"]
            [list "the" "Det"] 
            [list "thing" "N"]
            [list "over" "P"]
            [list "hedge" "N"]
            ))

;; Test phrase using command line arguments
;(define wlist
;    (vector->list (current-command-line-arguments)))
(define wlist (list "I" "see" "the" "thing"
               ))
;; (display (tag "I" l))

;; (type_of phr l)  ;; return the phrase type
;; (type_all wlist '() '() l)  ;; return the phrase type

;; (sentence? wlist l)  ;; is wlist a sentence?



