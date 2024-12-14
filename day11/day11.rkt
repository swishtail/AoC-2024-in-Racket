#lang racket
;;; Advent of Code - Day 11: Plutonian Pebbles
(define input
  (file->list "input"))

;;; Part One
(define (number-of-digits n)
  (inexact->exact (floor (+ (log n 10) 1))))

(define (split-in-two n n-digits)
  (let ((p (expt 10 (/ n-digits 2))))
    (list (floor (/ n p))
          (modulo n p))))

(define (rules . args)
  (let recurse ((remaining args))
    (if (null? remaining)
        '()
        (if (zero? (car remaining))
            (cons 1
                  (recurse (cdr remaining)))
            (let* ((n (car remaining))
                   (num-digits (number-of-digits n)))
              (if (even? num-digits)
                  (let ((num-split (split-in-two n num-digits)))
                    (cons (car num-split)
                          (cons (cadr num-split)
                                (recurse (cdr remaining)))))
                  (cons (* n 2024)
                        (recurse (cdr remaining)))))))))

(define (blink n rules stones)
  (if (zero? n)
      stones
      (blink (- n 1)
             rules
             (apply rules stones))))

(define (part-one x)
  (length (blink 25 rules x)))

(display
 (part-one input))
(newline)
