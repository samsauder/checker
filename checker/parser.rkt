;; Sam Sauder
;; 10-11-24
;; Purpose: implementing an LL(1) parser for simple English sentences

;; V = {S, Vp, Np, Np*} sentence, verb phrase, noun phrase, noun phrase (nullable)
;; Σ = {v, n, d}  verb, noun, determiner
;; R = { .. }

#lang racket
(provide S Vp Np Np*)


;; PRODUCTION RULE FUNCTIONS
;; =============================================================
;; ARGS: a tags-string (w)
;; For production rules for variable X:
;;      if w can be derived from X: return #t
;;      else: return #f
;; =============================================================

;; S -> NpVp
(define (S w)
    (define i (Vp_index w))  ;; index of the char after the last char in the Np

    (cond
        [(eq? i -1)  #f]  ;; no Np
        [else
              (Vp (substring w i (string-length w)))]
        )
  )


;; Return the index of the first character of the Vp in w (assuming there was a previous Np)
;; or -1 if there is no Np
(define (Vp_index w)
    (define l (string-length w))  ;; length of tags-string, w

    (cond
        [{and (> l 1) (Np (sub w 1))}  1]  ;; substring up to 1 is a Np
        [{and (> l 2) (Np (sub w 2))}  2]  ;; substring up to 2 is a Np
        [else -1]))


;; Vp -> vNp*
(define (Vp w)
  (cond
    [(equal? (sub w 1) "v")
        (Np* (substring w 1))]  ;; is the rest of w a valid complement? 
    
    [else  #f]
    )
  )


;; Np -> n | dn
;; PRECONDITION: length of w is at least 1
(define (Np w)
    (cond
      [(n? (sub w 1)) #t]  ;; is the first tag noun? 
      [(d? (sub w 1))      ;; is the first tag a determiner? 
        (n? (substring w 1))]  ;; is the complement a noun?
      [else #f]))


;; Np* -> Np | e
(define (Np* w)
  (cond
    [(equal? w "")  #t]
    
    [else (Np w)]
    )
  )


;; terminal testers
(define (n? w)
    (if (equal? w "n") #t #f))

(define (v? w)
    (if (equal? w "v") #t #f))

(define (d? w)
    (if (equal? w "d") #t #f))


;; HELPER FUNCTIONS
;; ARGS: a tags-string (w), the end index of the substring 
;; Returns the substring of w from 0 to end_index - 1 
(define (sub w end_index)
    (substring w 0 end_index))
