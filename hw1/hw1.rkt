#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))


; Exercise 1 - Define dupls-removed
(define (dupls-removed sent)
  ; Your code here
  (if (null? sent)
      '()
      (if (member (car sent) (cdr sent))
          (dupls-removed (cdr sent))
          (cons (car sent) (dupls-removed (cdr sent))))))


; Exercise 2 - Define count-word
(define (count-word sent wd)
  ; Your code here
  (cond
   ((null? sent) 0)
   ((equal? (car sent) wd) (+ 1 (count-word (cdr sent) wd)))
   (else (count-word (cdr sent) wd))))


; Exercise 3
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))


; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here

Використання new-if замість вбудованого if,
призводить до застрягання у так званому нескінченному циклі для слів, які не починаються з голосної.
Тому що new-if пропускав би як правильні так і неправильні випадки, постійно викликаючи pigl.

|#


; Exercise 4 - Define squares
(define (squares sent)
  ; Your code here
  (if (null? sent)
      '()
      (cons (* (car sent) (car sent)) (squares (cdr sent)))))


; Exercise 5 - Define switch
(define (switch sent)
  ; Your code here
  (let ((switched (switch-helper sent)))
    (if (equal? (car sent) 'you)
        (cons 'I (cdr switched))
        switched)))

(define (switch-helper sent)
  (if (null? sent)
      '()
      (let ((word (car sent)))
        (cons (cond
               ((equal? word 'I) 'you)
               ((equal? word 'me) 'you)
               ((equal? word 'you) 'me)
               (else word))
              (switch-helper (cdr sent))))))


; Exercise 6 - Define ordered?
(define (ordered? sent)
  ; Your code here
  (if (or (null? sent) (null? (cdr sent)))
      #t
      (if (> (car sent) (cadr sent))
          #f
          (ordered? (cdr sent)))))


; Exercise 7 - Define ends-e
(define (ends-e sent)
  ; Your code here
  (if (null? sent)
      '()
      (if (equal? (last (string->list (symbol->string (car sent)))) #\e)
          (cons (car sent) (ends-e (cdr sent)))
          (ends-e (cdr sent)))))


; Exercise 8
; Most versions of Lisp provide and and or procedures like the ones we've seen.
; In principle, there is no reason why these can't be ordinary procedures, but some versions of Lisp make them special forms.
; Suppose, for example, we evaluate (or (= x 0) (= y 0) (= z 0)). If or is an ordinary procedure,
; all three argument expressions will be evaluated before or is invoked. But if the variable x has the value 0,
; we know that the entire expression has to be true regardless of the values of y and z.
; A Lisp interpreter in which or is a special form can evaluate the arguments one
; by one until either a true one is found or it runs out of arguments.
;
;
; Devise a test that will tell you whether Racket's and and or are special forms or ordinary functions.
; This is a somewhat tricky problem, but it'll get you thinking about the evaluation process more deeply.
; Why might it be advantageous for an interpreter to treat or as a special form and evaluate its arguments one at a time?
; Can you think of reasons why it might be advantageous to treat or as an ordinary function?


#|

Your explanation here
(or #t (/ 1 0))
(and #f (/ 1 0))

(or (empty? nums) (empty? (bf nums)))
(and (integer? x) (even? x))


benefits
;; 1: getting desired values instead of #t or #f
;; 2: avoid unnecessay or wrong computation
;; 3: avoid else for a more specific condition
;; disadvantages
;; 1: the next expression needs to be evaluated

|#