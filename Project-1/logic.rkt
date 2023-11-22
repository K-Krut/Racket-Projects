#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))


(define (pattern-matcher arg-pattern arg-sent)
  (define (get-everything-after-pattern temp-pattern temp-sent)
      (cond
        ((empty? temp-pattern)  temp-sent)
        ((empty? temp-sent) #f)
        ((equal? (first temp-pattern) (first temp-sent)) (get-everything-after-pattern (bf temp-pattern) (bf temp-sent)))
        (else (get-everything-after-pattern arg-pattern (bf temp-sent)))))
  (define (get-everything-before-pattern temp-pattern temp-sent)
      (cond
        ((empty? temp-pattern)  temp-sent)
        ((empty? temp-sent) #f)
        ((equal? (last temp-pattern) (last temp-sent)) (get-everything-before-pattern (bl temp-pattern) (bl temp-sent)))
        (else (get-everything-before-pattern arg-pattern (bl temp-sent)))))
    (list (get-everything-before-pattern arg-pattern arg-sent) arg-pattern (get-everything-after-pattern arg-pattern arg-sent)))



(define (check-for-replace one-elem arg-from arg-to)
     (cond
        ((or (empty? arg-from) (empty? arg-to)) one-elem)
        ((equal? one-elem (first arg-from)) (first arg-to))
        (else (check-for-replace one-elem (bf arg-from) (bf arg-to)))))