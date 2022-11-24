#lang pl


;------------------------------------------------------------------------------

#|

This is the homework of Avihay Barnholtz
All the tests for all the question are in the end of the file.

|#





;------------------------------------------------------------------------------


#|

In the function "open-list" I used the function 'append' for adding
two lists and I activated it recursivly on the list of lists.
In the end ,the result is a big list with all the elemnets of the inner lists.

|#

;Qustion 1a
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lists)
     (cond
         [(null? lists) '()]
         [else (append (first lists) (open-list (rest lists)))]
         )
  
)


#|

In the function "min&max" I used the helping functions that I built 'min_of_list'
and 'max_of_list'. I used the function open-list from the first sub-question to organize all the elements,
and then I passed it to the helping functions to find the minimum  and maximum.

In the function "min&max_apply" things are much easier and instead of using the helping function I just used
the built-in 'apply' function to find the minimum and maximum.

|#

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



#|

I defined the Table object with two constructors.

In the function "search-table" I recursivly search for the input symbol,
 and if it exists i returned it and if it dont I returned #f (because the function will stop when the input table will
be empty).

In the function "remove-item" I search for the input symbol and if I find it
I skiped on it and concated the table until the symbol with the table after.
If the symbol is not in the table the functions returns the same table.

|#


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
(test (min&max '(() (3))) => '(3.0 3.0))
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