#lang racket

(require (planet dyoo/simply-scheme))
(require math/number-theory)
(provide (all-defined-out))



; Exercise 1 - Define substitute
(define (substitute sent old-word new-word)
  ; Your code here
  (map (lambda (word)
         (if (equal? word old-word)
             new-word
             word))
       sent))


#| ###################################################################################################### |#


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: #<procedure>

((lambda (x) (+ x 3)) 7)
-> returns: 10

(define (make-adder num)
  (lambda (x) (+ x num)))
((make-adder 3) 7)
-> returns: 10

(define plus3 (make-adder 3))
(plus3 7)
-> returns: 10

(define (square x) (* x x))
(square 5)
-> returns: 25

(define square (lambda (x) (* x x)))
(square 5)
-> returns 25

(define (try f) (f 3 5))
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


#| ###################################################################################################### |#


; Exercise 3
#|
процедура g - приймає іншу процедуру
Number of arguments g has: 0
Type of value returned by g: функція

|#


#| ###################################################################################################### |#


; Exercise 4 - Define f1, f2, f3, f4, and f5
#|
 For each expression, give a definition of f such that evaluating the expression will not
cause an error, and say what the expression's value will be, given your definition.
To be clear, for number one, define f1, for number 2, define f2, etc.

f1
(f2)
(f3 3)
((f4))
(((f5)) 3)

|#

(define f1 2)
;; -> f1
;; -> returns: 2

(define f2 +)
;; -> (f2)
;; -> returns:  #<procedure

(define (f3 x) (* x x))
;; -> (f3 3)
;; -> returns: 9

(define f4 (lambda () (lambda () (* 4 4))))
;; -> ((f4))
;; -> returns: 16

(define f5 (lambda () (lambda () (lambda (x) (* x x)))))
;; -> (((f5)) 5)
;; -> returns: 25


#| ###################################################################################################### |#


; Exercise 5 - Try out the expressions
(define (t f)
  (lambda (x) (f (f (f x)))) )
#|
1. ((t add1) 0) returns: 3
2. ((t (t add1)) 0) returns: 9
3. (((t t) add1) 0) returns: 27
|#


#| ###################################################################################################### |#


; Exercise 6 - Try out the expressions
(define (s x)
  (+ 1 x))

#|
1. ((t s) 0) returns: 3
2. ((t (t s)) 0) returns: 9
3. (((t t) s) 0) returns: 27
|#


#| ###################################################################################################### |#


; Exercise 7 - Define make-tester
#|
Write and test the make-tester procedure. Given a word w as its argument,
make-tester returns a procedure of one argument x that returns true if x is equal to w and false otherwise.
|#
(define (make-tester wd)
  ; Your code here
  (lambda (x)
    (equal? x wd)))


#| ###################################################################################################### |#


; Exercise 8 - SICP exercises

; SICP 1.31a
#|
The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order
procedures.51 Write an analogous procedure called product that returns the product of the values of a function at
points over a given range. Show how to define factorial in terms of product. Also use product to compute
approximations to π
|#
(define (product term a next b)
  ; Your code here
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))


(define (estimate-pi)
  (define (square x) (* x x))
  (define (add2 x) (+ 2.0 x))
  (/ (* 8.0 (product square 4.0 add2 100)) (* 100 (product square 3.0 add2 100))))


; SICP 1.32a
#|
This is called my-accumulate so it doesn't conflict with Simply
Show that sum and product (Exercise 1.31) are both special cases of a still more general notion called accumulate
that combines a collection of terms, using some general accumulation function:
   (accumulate
     combiner null-value term a next b)
Accumulate takes as arguments the same term and range specifications as sum and product,
together with a combiner procedure (of two arguments) that specifies how the current term is to be
combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when
the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate.
|#
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  ; Your code here
  (if (> a b)
      null-value
      (combiner (term a)
                (my-accumulate combiner null-value term (next a) next b))))


;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  ; Your code here
  (my-accumulate + 0 term a next b))

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  ; Your code here
  (my-accumulate * 1 term a next b))


; SICP 1.33
#|
You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion of a filter on
the terms to be combined. That is, combine only those terms derived from values in the range that satisfy
a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate,
together with an additional predicate of one argument that specifies the filter.
Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:

1. the sum of the squares of the prime numbers in the interval a to b
    (assuming that you have a prime? predicate already written)

2. the product of all the positive integers less than n that are relatively prime to n
    (i.e., all positive integers i<n such that GCD(i,n)=1).
|#
(define (filtered-accumulate combiner null-value term a next b pred)
  ; Your code here
  (if (> a b)
      null-value
      (combiner (if (pred a) (term a) null-value)
                (filtered-accumulate combiner null-value term (next a) next b pred))))


(define (sum-sq-prime a b)
  ; Your code here
  (filtered-accumulate + 0 (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b prime?))

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  ; Your code here
  (filtered-accumulate * 1 identity 1 (lambda (x) (+ x 1)) (- n 1) (lambda (i) (rel-prime? i n))))



; SICP 1.40 - Define cubic
#|
Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form
(newtons-method (cubic a b c) 1)
to approximate zeros of the cubic x3+ax2+bx+c
|#
(define (cubic a b c)
  ; Your code here
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))


; SICP 1.41 - Define double
#|
Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies
the original procedure twice. For example, if inc is a procedure that adds 1 to its argument,
then (double inc) should be a procedure that adds 2. What value is returned by
(((double (double double)) inc) 5)
|#
(define (double proc)
  ; Your code here
  (lambda (x)
    (proc (proc x))))


; SICP 1.43 - Define repeated
#|
If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f,
which is defined to be the function whose value at x is f(f(…(f(x))…)). For example, if f is the function x↦x+1,
then the nth repeated application of f is the function x↦x+n. If f is the operation of squaring a number, then the nth
repeated application of f is the function that raises its argument to the 2n-th power.
Write a procedure that takes as inputs a procedure that computes f and a positive integer n
and returns the procedure that computes the nth repeated application of f.

Your procedure should be able to be used as follows:
((repeated square 2) 5)
625
|#
(define (my-repeated proc n)
  ; Your code here
  (if (= n 1)
      proc
      (lambda (x) (proc ((my-repeated proc (- n 1)) x)))))


#| ###################################################################################################### |#


; Exercise 9 - Define my-every
#|
Last week you wrote procedure squares, that squared each number in its argument sentence, and saw pigl-sent,
that pigled each word in its argument sentence. Generalize this pattern to create a higher order procedure
called my-every that applies an arbitrary procedure, given as an argument, to each word of an argument sentence.
-> (my-every square '(1 2 3 4))
(1 4 9 16)
-> (my-every first '(nowhere man))
(n m)
|#
(define (my-every proc sent)
  ; Your code here
  (map proc sent))


#| ###################################################################################################### |#


; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns: '(pp uu rr pp ll ee

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns: '(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns: '(76 24)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns: 'ooeee

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns: ""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns:  Invalid arguments to MEMBER?:  'purple 'aeiou

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns: '(purple)
|#