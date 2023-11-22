#lang racket


;; ТІ-01 Круть Катерина Олександрівна


(require (planet dyoo/simply-scheme))
(require math/number-theory)
(provide (all-defined-out))



; Exercise 1 - Define fast-expt-iter
(define (square x)
  (* x x))

(define (fast-expt-iter b n)
  ; Your code here
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))


#| ###################################################################################################### |#


; Exericse 2 - Define phi

(define (phi)
  ; Your code here
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


#| ###################################################################################################### |#


; Exercise 3 - Define cont-frac
;; Recursive version
(define (cont-frac n d k)
  ; Your code here
  (define (iter m)
    (if (> m k)
        0.0
        (/ (n m) (+ (d m) (iter (+ m 1))))))
  (let ((result (iter 1)))
    (if (= result (round result))
        (round result)
        result)))


;; Iterative version
(define (cont-frac-iter n d k)
;   Your code here
    (define (iter counter result)
      (if (= counter 0)
          result
          (iter (- counter 1) (/ (n counter) (+ (d counter) result)))))
    (iter k 0.0))




(define (e k)
  ; Your code here to estimate e using cont-frac with k terms.
  (+ (cont-frac
       (lambda (i) 1.0)
       (lambda (i) (if (= (remainder i 3) 2)
                       (+ 2.0 (* 2.0 (quotient i 3)))
                       1.0)) k) 2.0))


#| ###################################################################################################### |#


; Exercise 4 - Define next-perf
(define (next-perf n)
  (define (sum-of-factors counter)
    (cond [(<= counter 0) 0]
          [(= (remainder n counter) 0)
           (+ counter (sum-of-factors (- counter 1)))]
          [else (sum-of-factors (- counter 1))]))
  (if (and (= (sum-of-factors (- n 1)) n) (not (= n 0)))
      n
      (next-perf (+ n 1))))


#| ###################################################################################################### |#


; Exercise 5 - Explain what happens when the base cases are interchanged.
#|
Here is the definition of count-change program from earlier in this lesson:
(define (count-change amount)
  (cc amount `(50 25 10 5 1)))

(define (cc amount kinds-of-coins)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (empty? kinds-of-coins)) 0]
        [else (+ (cc amount
                     (bf kinds-of-coins))
                 (cc (- amount
                        (first kinds-of-coins))
                     kinds-of-coins))] ))

Explain the effect of interchanging the order in which the base cases in the cc procedure are checked.
That is, describe completely the set of arguments for which the original cc procedure would return
a different value or behave differently from a cc procedure coded as given below,
and explain how the returned values would differ.
(define (cc amount kinds-of-coins)
  (cond
    [(or (< amount 0) (empty? kinds-of-coins)) 0]
    [(= amount 0) 1]
    [else ... ] ) ) ; as in the original version
|#


#|
Your explanation here
якщо сума дорівнює нулю, а поле kinds-of-coins порожнє, виводиться дві різні відповіді
|#


#| ###################################################################################################### |#


; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.
#|
Here is the iterative exponentiation procedure from earlier in this lesson:

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))
Give an algebraic formula relating the values of the parameters b, n, counter,
and product of the iterative exponentiation procedure defined above.

(The kind of answer we're looking for is "the sum of b, n, and counter times product
is always equal to 37.")
|#



#|
Formula for expt:
b^n = b^n
Formula for expt-iter:
b^counter*product = b^n
|#