#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt")
(require "logic.rkt")

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.
(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent)
    ;;insert your answer here
    sent)


;;Q2 - stupidbot-creator
  (define (stupidbot-creator motto)
    ;;insert your answer here
    (lambda (sent) motto))


;;Q3 - matcherbot-creator
  (define (matcherbot-creator pattern)
    ;;insert your answer here
    (lambda (sent) (last (pattern-matcher pattern sent))))


;;Q4 - substitutebot-creator
  (define (substitutebot-creator from to)
    ;;insert your answer here
    (define (substitute-bot sent)
      (if  (empty? sent)
           '()
           (se (check-for-replace (first sent) from to) (substitute-bot (bf sent)))))
    (lambda (arg-sent) (substitute-bot arg-sent)))


;;Q5 - switcherbot
  (define (switcherbot sent)
    ;;insert your answer here
    (define (switch-helper-func arg-sent)
      (if  (empty? arg-sent)
           '()
           (se (check-for-replace (first arg-sent) '(me I am was my yours you are were your mine) '(you you are were your mine me am was my yours)) (switch-helper-func (bf arg-sent)))))
    (if (equal? (first sent) 'you)
          (se 'I (switch-helper-func (bf sent)))
          (switch-helper-func sent)))


;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    ;;insert your answer here
    (if (empty? sent)
        '()
        (se (switcherbot sent) '?)))


;;Q7 - eliza
  (define (eliza sent)
    ;;insert your answer here
    (cond
      ((empty? sent) '(how can I help you ?))
      ((list? (first (pattern-matcher '(hello) sent))) '(hello there!))
      ((equal? (last sent) '?) '(I can not answer your question.))
      ((and (>= (count sent) 2) (equal? (se (first sent) (first (bf sent))) '(I am))) (se '(why are you) (bf (bf sent)) '?))
      (else  (switcherbot sent))))


;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    ;;insert your answer here
    (define (reactorbot sent)
      (if (equal? sent pat)
       out
       (bot sent)))
    reactorbot)

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    ;;insert your answer here
    (define (replacerbot sent)
      (define check-pattern (pattern-matcher pat sent))
      (if (list? (first check-pattern))
       (se before (last check-pattern) after)
       (bot sent)))
    replacerbot)

;;Q10 - exagerate
  (define (exaggerate bot n)
    ;;insert your answer here
    (define (exaggerate-helper arg-sent)
      (cond
        ((empty? arg-sent) '())
        ((adjective? (first arg-sent)) (se 'very (first arg-sent) (exaggerate-helper (bf arg-sent))))
        (else (se (first arg-sent) (exaggerate-helper (bf arg-sent))))))
    (define (helper-function arg-n x arg-proc arg-proc-additional)
      (if (<= arg-n 0)
        x
        (arg-proc-additional (arg-proc (helper-function (- arg-n 1) x arg-proc arg-proc-additional)))))
    (lambda (arg-sent) (helper-function n arg-sent bot exaggerate-helper)))