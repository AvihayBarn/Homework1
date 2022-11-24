#lang pl



;Qustion 1a
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lists)
     (cond
         [(null? lists) '()]
         [else (append (first lists) (open-list (rest lists)))]
         )
  
)

(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test (open-list '()) => '())
(test (open-list '(() (2 3 3 4) (9 2 -1) (233 11 90))) => '(2 3 3 4 9 2 -1 233 11 90))
(test (open-list '((1 2 3) () (9 2 -1) ())) => '(1 2 3 9 2 -1))
(test (open-list '(() () () (9))) => '(9))


;Question 1b

(: min_of_list : (Listof Number) -> Number)
(: max_of_list : (Listof Number) -> Number)
(: min&max : (Listof(Listof Number)) -> (Listof Number))
(: min&max_apply : (Listof(Listof Number)) -> (Listof Number))


;Helping functions:

(define (min_of_list list)
  (cond
         [(eq? (length list) 1) (min (first list) +inf.0)]
         [else (min (first list) (min_of_list (rest list)))]
         )
  
 )


(define (max_of_list list)
    (cond
         [(eq? (length list) 1) (max (first list) -inf.0)]
         [else (max (first list) (max_of_list (rest list)))]
         )
 
 )
(define (min&max lists)
  (cond
    [(null? lists) '(-inf.0 +inf.0)]
    [else (list (min_of_list (open-list lists)) (max_of_list (open-list lists)))]
  )
  
)


(define (min&max_apply lists)
  (cond
    [(null? lists) '(-inf.0 +inf.0)]
    [else (list (apply min (open-list lists)) (apply max (open-list lists)))]
  )
  
)

;Question 1c


;Tests
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233
11 90))) => '(-1 233))



