;; Sam Sauder
;; 10-11-24
;; Purpose: implementing an LL(1) parser for simple English sentences

;; V = {S, Vp, Np, Np*} sentence, verb phrase, noun phrase, noun phrase (nullable)
;; Σ = {v, n, d}  verb, noun, determiner
;; R = { .. }

#lang racket
(provide Vp Np Np*)

;; For production rules for variable X, with input of word w:
;;      if w can be derived from X: return #t
;;      else: return #f


;; PRODUCTION RULE FUNCTIONS
;; =========================


;; S -> NpVp
(define (S w)
    (define i (Vp_index w))  ;; index of the first char of the Vp

    (cond 
        [(not (equal? i -1))  ;; begins with a Np 
            (Vp (substring w i (string-length w)))]
        [else #f]  ;; does not begin with a Np
      )
  )


;; Return the index of the first character of the Vp in w (assuming there was a previous Np)
;; or -1 if there is no Np
(define (Vp_index w)
    (cond
        [{and (> (string-length w) 1) 
              (equal? (substring w 0 1) "n")}
            1]
        [{and (> (string-length w) 2)
              (equal? (substring w 0 2) "dn")}
            2]
        [else -1] 
      )
  )

;; Vp -> vNp*
(define (Vp w)
  (cond
    [(equal? (substring w 0 1) "v") 
        (Np* (substring w 1))]
    [else #f]
    )
  )


;; Np -> dn
;; Np -> n
(define (Np w)
  (cond
    [(equal? w "dn") #t]
    [(equal? w "n") #t]
    [(equal? w "") #t]
    [else #f]
    )
  )


;; Np* -> Np | e
(define (Np* w)
  (cond
    [(null? w) #t]
    [else (Np w)]
    )
  )
