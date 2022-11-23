#lang pl




(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lists)
     (cond
         [(null? lists) '()]
         [else (append (first lists) (open-list (rest lists)))]
         )
  
)

(open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11
90)))