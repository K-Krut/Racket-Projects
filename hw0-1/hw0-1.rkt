#lang racket


;; ТІ-01 Круть Катерина Олександрівна



(require (planet dyoo/simply-scheme))
(provide (all-defined-out))


;; Exercise 0 - Introduce yourself

#|This is a comment that spans multiple lines.
1) What is your name?
2) What is your major?
3) Are you a returning student? (i.e. Did you take 61AS last semester?)
4) What made you to take 61AS?
5) Tell us interesting things about yourself.|#


;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))
(define (sum-of-squares a b)
  (+ (square a) (square b)))

;; Exercise 2a - Define can-drive
(define (can-drive age)
  (if (< age 16)
      '(Not yet)
      '(Good to go)))


;; Exercise 2b - Define fizzbuzz
(define (fizzbuzz num)
  (cond ((and (= (remainder num 3) 0) (= (remainder num 5) 0)) 'fizzbuzz)
         ((= (remainder num 3) 0) 'fizz)
         ((= (remainder num 5) 0) 'buzz)
         (else num)))

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|Your answer here|#

;; Exercise 4 - new-if vs if
(define (infinite-loop) (infinite-loop))

(define (new-if test then-case else-case)
  (if test
    then-case
    else-case))
#|Your answer here|#