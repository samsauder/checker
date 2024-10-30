;; Sam Sauder
;; 10/29/24

#lang racket
(provide S)


;; This parser accepts all strings in the language L(G) where...
;;
;; Context Free Grammar, G = (V, Σ, R, S)
;; ===========================================
;; terminals (Σ) = {d, n, v, p, aj, av}
;;
;; variables (V) = {S, Np, Np^, Pp, Pp*, Vp}
;;
;; rules (R) = {
;;              S   -> Np Vp 
;;              Np  ->   d Np^ | n
;;              Np^ ->    aj n | n
;;              X*  ->  Np Pp* | av | e
;;              Pp  ->  p Np
;;              Pp* ->  Pp | e 
;;              Vp  ->  v X*
;;              }
;;
;; start variable (S) 
;; ===========================================
;; d: determiner
;; n: noun
;; v: verb
;; p: preposition
;; aj: adjective
;; av: adverb
;; ===========================================


;; S -> Np Vp
(define (S tags) 
    (define NounPhrase (Np tags))
    (define VerbPhrase (Vp (tag-break tags 'v)))

    (append NounPhrase VerbPhrase)
    )


;; Np -> d Np^ | n
(define (Np tags)
   (cond
        [(null? tags) 
            (error "[PARSER]  Syntax Error: tags do not form a Noun Phrase")]

        [(starts_with? tags 'd) 
            (append '(d) (Np^ (cdr tags)))]

        [(starts_with? tags 'n)
            '(n)]

        [else 
            (error "[PARSER]  Syntax Error: tags do not form a Noun Phrase")] 
    )
  )


(define (N tags)
  (if (equal? (car tags) 'n)
      '(n)
      (error "[PARSER]  Syntax Error: tag is not a noun")
    )
  )


;; Np^ -> aj n | n
(define (Np^ tags)
    (cond
        [(null? tags) 
            (error "[PARSER]  Syntax Error: tags do not form a Noun Phrase") 
         ]

        [(starts_with? tags 'aj) 
            (append '(aj) (N (cdr tags))) 
         ]

        [(starts_with? tags 'n) 
            '(n)
         ]

        [else
            (error "[PARSER]  Syntax Error: tags do not form a Noun Phrase")] 
      )
  )


;; X* -> Np Pp* | av | e
(define (X* tags)
    (cond  
        [(null? tags) null]

        [(starts_with? tags 'av)
         '(av)]

        [{or (starts_with? tags 'n)
             (starts_with? tags 'd)}
            (append (Np tags)
                    (Pp* (tag-break tags 'p)))]

        [else (error "[PARSER]  Syntax Error: tags do not form a Verb Phrase")]
      ) 
  )


;; Pp -> p Np
(define (Pp tags)
    (cond
        [(null? tags) 
            (error "[PARSER]  Syntax Error: tags do not form a Prepositional Phrase")]

        [(starts_with? tags 'p)
            (append '(p) (Np (cdr tags)))] 
        
        [else (error "[PARSER]  Syntax Error: tags do not form a Prepositional Phrase")]
      )
  )


;; Pp* -> Pp | e 
(define (Pp* tags)
    (cond 
        [(null? tags) null]

        [(starts_with? tags 'p)
            (Pp tags)]

        [else (error "[PARSER]  Syntax Error: tags do not form a Verb Phrase")]
      )
)


;; Vp -> v X*
(define (Vp tags)
    (cond
        [(null? tags) 
            (error "[PARSER]  Syntax Error: tags do not form a Verb Phrase")]

        [(starts_with? tags 'v) 
            (append '(v) (X* (cdr tags)))]

        [else (error "[PARSER]  Syntax Error: tags do not form a Verb Phrase")]
      )
  )


;; Helper functions
;; ====================================================================================

;; ARGS: a list of tags, a target tag 
;; Return the sublist of the given tag list beginning immediately before the first instance of tag
(define (tag-break tags tag)
    (cond
        [(null? tags) null]

        [(starts_with? tags tag)
            tags]

        [else
            (tag-break (cdr tags) tag)])
    )


;; ARGS: a list of tags, a tag
;; Returns #t if the first element of tags is tag, #f otherwise
(define (starts_with? tags tag)
    (if (equal? (car tags) tag) #t #f))


;; Command usage
;; ====================================================================================
(define (usage)
    (display "=========================================================================\n");
    (display "[PARSER]  Usage\n")
    (display "=========================================================================\n\n");
    (display "Terminals are represented as symbols.\n")
    (display "Select from: 'd 'n 'v 'p 'aj 'av\n")
    (display "Call the sentence procedure: (S [some list of symbols])\n\n")

    (display "If the list of symbols is a sentence, the original list will be returned.\n")
    (display "Otherwise, the parser will output a syntax error.\n\n")
    (display "=========================================================================\n");
  )
