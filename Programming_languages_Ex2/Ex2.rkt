#lang pl 02

#|
;==== Q.1(A): ====
(from the lecture)
<D> ::=
        |<Digit>                               1
        |<Digit><D>                            2
                                               
<SE> ::=
        |<Num>                                 3                          
        |<String>                              4
        |<Char>                               15
<String> ::=
        |{string <Char>}                       5
        |{number->string <Num>}                6
        |{string-append <String> <String>}     7
        |{string-insert <String> <Char> <Num>  8
        |"<D>"                                 9
        |<𝜆>                                  10
             
<Num> ::=
         |<D>                                 11
         |{string-length <Sring>}             12
<Char> ::=
          |\#<Digit> <Char>                   13
          |\#<Digit>                          14
|#

;==== check for your examples: ====
;        ==== Q.1(B): ====
#|
"12344"
<SE>--4--> <String> --9--> "<D>" --2,2,2,2,1-> "12344"
12
<SE> --3--> <Num> --11--> <D> --2,2--> 12
( string #\1 #\2 #\4 )
 <SE> --4--> <String> --5--> {string <Char>} --13,13,14-->(string #\1 #\2 #\4 )
( string-append ( string #\1 #\2 #\4 ) "12" )
 <SE> --4--> <String> --7--> {string-append <String> <String>} --5,9--> {string-append {string <Char>} "<D>"}
 -13,13,14-2,1-> ( string-append ( string #\1 #\2 #\4 ) "12" )
( string-insert "1357" #\4 66 )
<SE> --4--> <String> --8--> {string-insert <String> <Char> <Num>} --9,14,11--> ( string-insert <"D"> #\4 <D> )
--2,2,2,1-2,1--> ( string-insert <"D"> #\4 66 )
( number->string 156879 )
<SE> --4--> <String> --6--> {number->string <Num>} --11-->( number->string <D>) --2,2,2,2,2,1-->( number->string 156879)
( number->string ( string-length "0033344" ) )
<SE> --4--> <String> --6--> {number->string <Num>}--12--> {number->string {string-length <Sring>}}--9-->{number->string {string-length <"D">}}
--2,2,2,2,2,2,1-->{number->string {string-length "0033344"}}
#\3
 <SE> --15--> <Char> --14--> #\3
|#
; ==== Q.2: ====

#|
square function which takes a number as input,
 and produces a number which is the square of the input number. 
|# 
(: square : Number -> Number)
(define (square myNum)
   (* myNum  myNum))

(test (square 5) => 25)
(test (square 0) => 0)
(test (square 1) => 1)
(test (square 10) => 100)
(test (square -5) => 25)

#|
sum-of-squares function which takes a list of numbers as input,
 and produces a number which is the sum of the squares of all of the numbers in the list. 
|# 
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares mylist)
  (foldl + 0 (map square mylist)))
  

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0 2 5)) => 29)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(7 10 1)) => 150)
(test (sum-of-squares '(5)) => 25)
(test (sum-of-squares '(1 1 1 1 1 1 1 1 1 1 1)) => 11 )

;==== Q.3(A): ====
#|
function createPolynomial that takes as arguments a list of 𝑘 numbers 𝑎0, … , 𝑎𝑘−1
and returns as output a function.
The returned function takes a number 𝑥0 and return the
value of the polynomial 𝑎0 ⋅ 𝑥0 + ⋯ + 𝑎𝑘−1 ⋅ 𝑥𝑛−1 at 𝑥0
|# 

(: createPolynomial : (Listof Number) -> (Number -> Number)) 
(define (createPolynomial mylist) 
  (: poly : (Listof Number) Number Integer Number -> 
Number) 
  (define (poly argsL x power accum) 
     (if (null? argsL)
         accum
         (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power))))        
         ) )
 (: polyX : Number -> Number)
  (define (polyX x)
    (poly mylist x 0 0))
polyX)


(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>
 (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5
(expt 0 3))))
(test (p2345 4) =>
 (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5
(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4
(expt 11 2)) (* 5 (expt 11 3))))
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6
(expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)
(define p-1-3-4-5 (createPolynomial '(-1 -3 -4 -5)))
(test (p-1-3-4-5 0) =>
 (+ (* -1 (expt 0 0)) (* -3 (expt 0 1)) (* -4 (expt 0 2)) (* -5
(expt 0 3))))
(test (p-1-3-4-5 7) =>
 (+ (* -1 (expt 7 0)) (* -3 (expt 7 1)) (* -4 (expt 7 2)) (* -5
(expt 7 3))))
(define p0330 (createPolynomial '(0 3 3 0)))
(test (p0330 1) =>
 (+ (* 0 (expt 1 0)) (* 3 (expt 1 1)) (* 3 (expt 1 2)) (* 0
(expt 1 3))))
(test (p0330 7) =>
 (+ (* 0 (expt 7 0)) (* 3 (expt 7 1)) (* 3 (expt 7 2)) (* 0
(expt 7 3))))


;==== Q.3(B): ====

#| 
  The grammar: 
    <PLANG> ::= {{poly <AEs>}{<AEs>}}
    <AEs>   ::=<AE>
       | <AE> <AEs>
    <AE>::=<num>
       | {+ <AE> <AE> }
       | {- <AE> <AE> }
       | {* <AE> <AE> }
       | {/ <AE> <AE> }
|#
;==== AST definition ====
 
(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE])
 
  (: parse-sexpr : Sexpr -> AE) 
  ;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse-sexpr-2 : (Listof Sexpr) -> (Listof AE)) ;AEs === list of AE
  ;; to convert list of s-expressions into list of AEs 
  (define (parse-sexpr-2 sexpr_list)
(if (null? sexpr_list)
        null
        (cons (parse-sexpr (first sexpr_list))(parse-sexpr-2 (rest sexpr_list)))
    )
    )

   
  (: parse : String -> PLANG) 
  ;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code 
        [(list left right) (if (and (list? left) (list? right))
                           (cond [(not (eq? (first left) 'poly)) (error 'parse "bad syntax")]
                                 [(null? (rest left)) (error 'parse "at least one coefficient is required in ~s" code)]
                                 [(null? right) (error 'parse "at least one point is required in ~s" code)]
                                 [else (Poly (parse-sexpr-2 (rest left)) (parse-sexpr-2 right))])
                          (error 'parse "bad syntax"))]
        [else (error 'parse "bad syntax")])
     )
  )
 
(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3)))) 
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{poly 7 19} {7 -1 -3 9}}") => (Poly (list (Num 7) (Num 19)) (list (Num 7) (Num -1) (Num -3) (Num 9)))) 
(test (parse "{{poly } {} }") =error> "parse: at least one coefficient is required in ((poly) ())") 
(test (parse "{{poly 7 7 7} {} }") =error> "parse: at least one point is required in ((poly 7 7 7) ())")


;==== Q.3(C): ====
;; evaluates AE expressions to numbers
(: eval : AE -> Number ) 
  (define (eval expr) 
    (cases expr 
      [(Num n)  n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))

#|
This func make the same as foldr
the reson we create this func is just becuse the "foldr" did not work on list(even throgh in racket documention the func can take list as parameter)
|#
 (: my_foldr : (Listof AE) -> (Listof Number) ) 
  (define (my_foldr mylist)
    (if(null? mylist) null
       (cons (eval (first mylist))(my_foldr (rest mylist))))
       )

#|
this funcion calculate the polynim on each number from a list
because we failed using let and map methods
|#
(: calc_polynum : ((Listof Number) (Listof Number) -> (Listof Number)))
(define (calc_polynum coefficient x)
  (let([calc_case (createPolynomial coefficient)])
  (if (null? x) null (cons (calc_case (first x)) (calc_polynum coefficient (rest x))))
  ))

  (: eval-poly : PLANG -> (Listof Number)) 
  (define (eval-poly p-expr)
        (cases p-expr 
          [(Poly coefficient x) ( calc_polynum (my_foldr coefficient) ( my_foldr x)) 
                             ]))
  (: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str)))

;tests:

(test (run "{{poly 1 2 3} {1 2 3}}")  
=> '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  
=> '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))  
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") 
=> '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 
{/ 27 9}}}") 
=> '(0 4 4))
(test (run "{{poly 1 2 3} {1 2 3}}")  
=> '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 0 0 0} {1 2 3}}")  
=> '(0 0 0))
(test (run "{{poly 0 0 0} {0 0 0}}")  
=> '(0 0 0)) 
(test (run "{{poly 1 2 3} {0 0 0}}")  
=> '(1 1 1))
(test (run "{{poly 10} {0}}")  
=> '(10))
(test (run "{{poly 10 20 30} {0 0 0}}")  
=> '(10 10 10))
(test (run "{{poly -10 -20 -30} {0 0 0}}")  
=> '(-10 -10 -10))
(test (run "{{poly -10 20 30} {0 0 0}}")  
=> '(-10 -10 -10))
(test (run "{{poly 10 -20 30} {0 0 0}}")  
=> '(10 10 10))



