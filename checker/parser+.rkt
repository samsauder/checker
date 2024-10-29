;; Sam Sauder
;; 10/28/24
;; Purpose: impelementing a parser with more productiion rules using a modular design, improving on parser.rkt

#lang racket
(provide Np Np* Vp)

;; LABELS:
;; ==========================
;; terminals: {d, n, v}
;; variables: {Np, Np*, Vp}
;; ==========================


;; ARGS: a list of tags (terminals), a production list (each element is the string  
;;   label for a terminal or a variable)
;; Returns #t if the sequence of tags follows the production list
(define (check tags prod_list) 
    "check"       
)


;; ARGS: a list of tags
;; Return a list (list A B) where:
;; * A: the first sublist of tags that is a Noun Phrase
;; * B: the sublist of tags immediately following the Noun Phrase
(define (Np tags)
    (cond
        [(equal? (car tags) "d")
            (define tup (Np (cdr tags)))  ;; tuple of next Np
            (define A (cons "d" (car tup)))
            (define B (cadr tup))
            (list A B)
            ] ;; Np -> dn

        [(equal? (car tags) "n")          
            (define A (list "n"))   
            (define B (cdr tags))
            (list A B) 
            ]  ;; Np -> n  
        [else (error "syntax error: not a Np")]
      )
  )


;; ARGS: a list of tags
;; Return a list (list A B) or (list null null) where:
;; * A: the first sublist of tags that is a Noun Phrase
;; * B: the sublist of tags immediately following the Noun Phrase
(define (Np* tags)
    (cond  ;; Np' -> Np | e
        [(null? tags) (list null null)]  ;; e 
        [else
          (Np tags)]    ;; Np
      ) 
  )


;; ARGS: a list of tags
;; Return a list (list A B) where:
;; * A: the first sublist of tags that is a Verb Phrase
;; * B: the sublist of tags immediately following the Verb Phrase
(define (Vp tags)
    (cond 
        [(equal? (car tags) "v")
            (define tup (Np* (cdr tags)))
            (define A (cons "v" (car tup)))
            (define B (cadr tup))
            (list A B)
            ]
        [else (error "syntax error: not a Vp")]
      )
  )



