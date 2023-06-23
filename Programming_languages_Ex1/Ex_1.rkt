#lang pl
;;!!!!!!! q1 !!!!!!!!
;; part a:
#|
open-list get Listof list and return Listof Number
the idea is recursive append list to the first list until we got null
look in the internet how to append two lists
param: Listof (Listof Number)
return: Listof number
|#
(: open-list : (Listof (Listof Number)) -> (Listof Number))
(define (open-list lst)
(cond [(null? lst) null]
      [else (append (first lst) (open-list (rest lst)))]))
(test (open-list '((1) (2))) => '(1 2))
(test (open-list '((1) ())) => '(1))
(test (open-list '(() (2))) => '(2))
(test (open-list '((1) (2 3))) => '(1 2 3))
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
;; part b:
#|
This function find the minimun in list
param: Listof Number
return: Number
recursive min
|#
;;"minList"
(: minList : (Listof Number) -> Number)
(define (minList lst)
(cond [(null? lst) +inf.0]
      [else (min (first lst) (minList (rest lst)))]))
#|
This function find the maximun in list
param: Listof Number
return: Number
recursive max
|#
;;"maxList:"
(: maxList : (Listof Number) -> Number)
(define (maxList lst)
(cond [(null? lst) -inf.0]
      [else (max (first lst) (maxList (rest lst)))]))

#|
This function find the minimun&maximun in Listof list by using the help function 'minList', 'maxList' and open-list.
param: Listof (Listof Number)
return: Listof number
|#
;;"mix&max:"
(: min&max : (Listof (Listof Number)) -> (Listof Number))
(define (min&max lst)
(list (minList(open-list lst)) (maxList(open-list lst))))
(test (min&max '((-1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1 2))) => '(1.0 2.0))
(test (min&max '((9 2 -3) (233 500 90))) => '(-3.0 500.0))
(test (min&max '((5) (2 4))) => '(2.0 5.0))
(test (min&max '((5) ())) => '(5.0 5.0))
(test (min&max '(())) => '(+inf.0 -inf.0))
(test (min&max '()) => '(+inf.0 -inf.0))

;; part c
#|
This function find the minimun&maximun in Listof list by using apply
param: Listof (Listof Number)
return: Listof number
|#
;;"min&max_apply"
(: min&max_apply : (Listof (Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
(cond [(null? lst) '(+inf.0 -inf.0)]
      [else (list (apply min (open-list lst) ) (apply max (open-list lst)))]))

(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
(test (min&max_apply '((1 2))) => '(1 2))
(test (min&max_apply '((9 2 -3.0) (233 500 90))) => '(-3.0 500.0))
(test (min&max_apply '((5.0) (2.0 4))) => '(2.0 5.0))
(test (min&max_apply '((5) ())) => '(5 5))
(test (min&max_apply '()) => '(+inf.0 -inf.0))

      
;;!!!!!!! q2 !!!!!!!!
;;part a:
#|
simple constractor create emptry Table
and 'AddToTable' is table with values
|#
(define-type Table 
  [EmptyTbl]
  [AddToTable Symbol String Table]
  )
(test (EmptyTbl) => (EmptyTbl))
(test (Table? (EmptyTbl)) => #t)
#|
Add function
pararm: Symbol (the key in the table)
pararm: String (the value)
pararm: Table (the table we insert to)
return: Table
the idea is just use constructor to build a new table with parameters
if the table is empty switch else: add as a sub table
|#
;;part b
(: Add : Symbol String Table -> Table)
(define (Add sym str table)
(AddToTable sym str table))

(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) => (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (Add 'b "B" (EmptyTbl)) => (Add 'b "B" (EmptyTbl)))

#|
search-table function
pararm: Symbol (the key to search)
pararm: Table (the table we search on)
return: String(if we found the key) else: #f (not found)
the idea is to search recursive 3 cases:
1.if empty table return false(we not found)
2.not empty if the key is equal return the value(string)
3.we want to search recursive in the sub-table 
else
|#
;;part c
(: search-table  : Symbol Table -> (U String #f))
(define (search-table sym table)
(cases table
  [(EmptyTbl) #f]
  [(AddToTable symTable strTable tableTable)
   (cond [(equal? symTable sym) strTable]
         [else (search-table sym tableTable)
         ])]))

(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> "AAA")
(test (search-table 'a (EmptyTbl))=> #f)
(test (search-table 'b (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> "B")
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'c "A" (EmptyTbl)))))=> "A")
#|
remove-item function
pararm: Table (the table we search on)
pararm: Symbol (the key to remove)
return: Table
the idea is like part c:
1.if empty table return empty table
2.not empty if the key is equal we want to remove it( return the sub table)
3.we want to add recursive the sub-table and check if is equal to key not added it.
|#
;;part d
(: remove-item  : Table Symbol -> Table)
(define ( remove-item table sym)
  (cases table
    [(EmptyTbl) (EmptyTbl)]
    [(AddToTable symTable strTable tableTable)
     (cond
       [(equal? symTable sym) tableTable]
       [else (Add symTable strTable (remove-item tableTable sym))
         ])]))

(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'a)=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b)=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "A" (EmptyTbl)) 'b)=> (AddToTable 'a "A" (EmptyTbl)))
(test (remove-item (Add 'a "A" (EmptyTbl)) 'A)=> (AddToTable 'a "A" (EmptyTbl)))
(test (remove-item (Add 'a "A" (EmptyTbl)) 'a)=> (EmptyTbl))