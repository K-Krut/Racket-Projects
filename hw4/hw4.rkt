#lang racket


;; ТІ-01 Круть Катерина Олександрівна


(require (planet dyoo/simply-scheme))
(require math/number-theory)
(provide (all-defined-out))



; Exercise 1
; SICP 2.7 - Define upper-bound and lower-bound
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))


(define (lower-bound interval)
(car interval))

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


; SICP 2.10 - Modify div-interval

(define (div-interval x y)
  (if (>= (* (upper-bound y) (lower-bound y)) 0.)
      (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y))))
      (error "divide by zero")))


;SICP 2.12 - Define make-center-percent and percent
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tol)
  (cons (- c (/ (* c tol) 100)) (+ c (/ (* c tol) 100))))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))


; SICP 2.17 - Define last-pair
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))



; SICP 2.20 - Define same-parity
(define (same-parity x . y)
  (cons x
        (if (even? x)
          (filter even? y)
          (filter odd? y))))


; SICP 2.22 - Write your explanation in the comment block:
#|
Your explanation here
перше число - це квадрат решти з відповіді попередньої ітерації
друге - відповідь з попередньої ітерації (список)

|#


#| ###################################################################################################### |#


; Exercise 2 - Define my-substitute
(define (substitute lst old new)
  (if (null? lst)
      '()
      (if (list? (car lst))
          (cons (substitute (car lst) old new) (substitute (cdr lst) old new))
          (if (equal? (car lst) old)
            (cons new (substitute (cdr lst) old new))
            (cons (car lst) (substitute (cdr lst) old new))))))



#| ###################################################################################################### |#


; Exercise 3 - Define my-substitute2
(define (substitute2 lst old new)
  (define (helper lst1 old1 new1)
    (if (null? lst1)
        '()
        (if (list? (car lst1))
            (cons (helper (car lst1) old new) (helper (cdr lst1) old new))
            (if (null? old1)
              (cons (car lst1) (helper (cdr lst1) old new))
              (if (equal? (car lst1) (car old1))
                (cons (car new1) (helper (cdr lst1) old new))
                (helper lst1 (cdr old1) (cdr new1)))))))
  (helper lst old new))
