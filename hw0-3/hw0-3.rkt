#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

; Exercise 0
(define (count-ums sent)
  ; your code here
  (cond
    ((null? sent) 0)
    ((equal? 'um (first sent)) (+ 1 (count-ums (bf sent))))
    (else (count-ums (bf sent)))))

(define (countdown num)
  ; your code here
  (if (= num 0)
      '(blastoff!)
      (cons num (countdown (- num 1)))))

(define (numbers sent)
  ; your code here
  (cond
    ((null? sent) '())
    ((number? (first sent))
     (cons (first sent) (numbers (bf sent))))
    (else (numbers (bf sent)))))


;; Exercise 1 - Define describe-time
(define (describe-time secs)
  ; your code here
   (cond
      ((< secs 60) (se secs 'seconds))
      ((< secs (* 60 60))
       (se (quotient secs 60) 'minutes (describe-time (remainder secs 60))))
      ((< secs (* 60 60 24))
       (se (quotient secs (* 60 60)) 'hours (describe-time (remainder secs (* 60 60)))))
      (else (se (quotient secs (* 60 60 24)) 'days (describe-time (remainder secs (* 60 60 24)))))))

;; Exercise 2 - Define remove-once
(define (remove-once item lst)
  ; your code here
  (cond ((null? lst) '())
        ((and (equal? (car lst) item) (not (member (car lst) (cdr lst))))
         (cdr lst))
        (else (cons (car lst) (remove-once item (cdr lst))))))

;; Exercise 3 - Define differences
(define (differences lst)
  ; your code here
  (if (null? (cdr lst))
      '()
      (cons (- (cadr lst) (car lst)) (differences (cdr lst)))))

;; Exercise 4 - Define location
(define (location word sentence)
  ; your code here
  (let loop ((sentence sentence) (index 1))
    (cond ((null? sentence) #f)
          ((eq? word (car sentence)) index)
          (else (loop (cdr sentence) (+ index 1))))))

;; Exercise 5 - Define initials
(define (initials sent)
  ; your code here
  (if (empty? sent)
      '()
      (se (first (first sent))
          (initials (bf sent)))))

;; Exercise 6 - Define copies
(define (copies num word)
  ; your code here
  (if (= num 0)
      '()
      (cons word (copies (- num 1) word))))

;; Exercise 7 - Define gpa
(define (gpa grades)
  ; your code here
  (define (base-grade grade)
    (cond ((equal? (first grade) 'A) 4.00)
          ((equal? (first grade) 'B) 3.00)
          ((equal? (first grade) 'C) 2.00)
          ((equal? (first grade) 'D) 1.00)
          ((equal? (first grade) 'F) 0.00)))
  (define (grade-modifier grade)
    (define (second wd) (first (bf wd)))
    (cond ((empty? (bf grade)) 0)
          ((equal? (second grade) '+) 0.33)
          ((equal? (second grade) '-) -0.33 )))
  (define (grade g) (+ (base-grade g) (grade-modifier g)))
  (if (empty? grades)
      0
      (/
       (+ (grade (first grades)) (* (gpa (bf grades)) (count (bf grades))))
       (+ 1 (count (bf grades))))))



;; Exercise 8 - Define repeat-words
(define (repeat-words sentence)
  ; your code here
  (if (null? sentence)
      '()
      (if (number? (car sentence))
          (append (copies (car sentence) (cadr sentence)) (repeat-words (cddr sentence)))
          (cons (car sentence) (repeat-words (cdr sentence))))))

;; Exercise 9 - Define same-shape?
(define (same-shape? s1 s2)
  ; your code here
  (and (= (length s1) (length s2))
       (let loop ((s1 s1) (s2 s2))
         (or (null? s1) (and (= (count (car s1)) (count (car s2))) (loop (cdr s1) (cdr s2)))))))

