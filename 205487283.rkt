#lang pl


;------------------------------------------------------------------------------

#|

This is the homework of Avihay Barnholtz
All the tests for all the question are in the end of the file.

|#



;Qustion 1a
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lists)
     (cond
         [(null? lists) '()]
         [else (append (first lists) (open-list (rest lists)))]
         )
  
)



;Question 1b

;Helping functions:
(: min_of_list : (Listof Number) -> Number)
(define (min_of_list list)
  (cond
         [(eq? (length list) 1) (min (first list) +inf.0)]
         [else (min (first list) (min_of_list (rest list)))]
         )
  
 )

(: max_of_list : (Listof Number) -> Number)
(define (max_of_list list)
    (cond
         [(eq? (length list) 1) (max (first list) -inf.0)]
         [else (max (first list) (max_of_list (rest list)))]
         )
 
 )

(: min&max : (Listof(Listof Number)) -> (Listof Number))
(define (min&max lists)
  (cond
    [(null? lists) '(-inf.0 +inf.0)]
    [else (list (min_of_list (open-list lists)) (max_of_list (open-list lists)))]
  )
  
)

;Question 1c

(: min&max_apply : (Listof(Listof Number)) -> (Listof Number))
(define (min&max_apply lists)
  (cond
    [(null? lists) '(-inf.0 +inf.0)]
    [else (list (apply min (open-list lists)) (apply max (open-list lists)))]
  )
  
)





;------------------------------------------------------------------------------






;Question 2.1

(define-type Table
  [EmptyTbl]
  [Add Symbol String Table]
 )


;Question 2.2
(: search-table : Symbol Table -> (U String #f))
(define (search-table isymbol itable)
  (cases itable
    [(EmptyTbl) #f]
    [(Add symbol string table) (cond
                                 [(eq? symbol isymbol) string]
                                 [else (search-table isymbol table)])
                               ]
    )
    )


;Question 2.3
(: remove-item : Table Symbol -> Table)
(define (remove-item itable isymbol)
  (cases itable
    [(EmptyTbl) (EmptyTbl)]
    [(Add symbol string table) (cond
                                 [(eq? symbol isymbol) table]
                                 [else (Add symbol string (remove-item table isymbol))])
                               ]
    )
    )





;------------------------------------------------------------------------------

;Tests




;Question 1 tests:
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test (open-list '()) => '())
(test (open-list '(() (2 3 3 4) (9 2 -1) (233 11 90))) => '(2 3 3 4 9 2 -1 233 11 90))
(test (open-list '((1 2 3) () (9 2 -1) ())) => '(1 2 3 9 2 -1))
(test (open-list '(() () () (9))) => '(9))
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '(() (3)) => '(3.0 3.0))
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233
11 90))) => '(-1 233))


;Question 2 tests
(test (EmptyTbl) => (EmptyTbl))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) =>
(Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) =>
(Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))))
=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))))
=> "AAA")
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))) 'a)
=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))) 'b)
=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))




;------------------------------------------------------------------------------