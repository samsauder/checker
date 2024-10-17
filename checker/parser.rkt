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
    (define wlen (string-length w))  ;; length of tags-string, w

    (cond
        ;; w is longer than 1 and the substring up to index 1 is "n" 
        [{and (> wlen 1) (equal? (sub w 1) "n")}  1]

        ;; w is longer than 2 and the substring up to index 2 is "dn"
        [{and (> wlen 2) (equal? (sub w 2) "dn")}  2]
        
        [else  -1] 
      )
  )


;; Vp -> vNp*
(define (Vp w)
  (cond
    [(equal? (sub w 1) "v")
        (Np* (substring w 1))]  ;; is the rest of w a valid complement? 
    
    [else  #f]
    )
  )


;; Np -> dn
;; Np -> n
(define (Np w)
  (cond
    [{or 
      (equal? w "dn") 
      (equal? w "n")}  #t]

    [else  #f]
    )
  )


;; Np* -> Np | e
(define (Np* w)
  (cond
    [(equal? w "")  #t]
    
    [else (Np w)]
    )
  )


;; HELPER FUNCTIONS
;; ARGS: a tags-string (w), the end index of the substring 
;; Returns the substring of w from 0 to end_index - 1 
(define (sub w end_index)
    (substring w 0 end_index))


