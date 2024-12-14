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

;;; Part Two
(define (list->table l)
  (if (null? l)
      '()
      (cons (cons (car l) 1)
            (list->table (cdr l)))))

(define (add-or-update-instance key value table)
  (if (null? table)
      (cons (cons key value)
            table)
      (if (equal? key (caar table))
          (cons (cons key (+ (cdar table) value))
                (cdr table))
          (cons (car table)
                (add-or-update-instance key value (cdr table))))))

(define (apply-rule n m intermediate)
  (let ((result
         (if (zero? n)
             1
             (let ((num-digits (number-of-digits n)))
               (if (even? num-digits)
                   (split-in-two n num-digits)
                   (* n 2024))))))
    (if (list? result)
        (add-or-update-instance
         (car result) m (add-or-update-instance
                         (cadr result) m intermediate))
        (add-or-update-instance result m intermediate))))

(define (traverse-list-with-rule stones)
  (let iterate ((result '())
                (remaining stones))
    (if (null? remaining)
        result
        (iterate (apply-rule (caar remaining)
                             (cdar remaining)
                             result)
                 (cdr remaining)))))

(define (blink-many n stones)
  (if (zero? n)
      stones
      (blink-many (- n 1)
                  (traverse-list-with-rule stones))))

(define (count-stones table)
  (apply + (map cdr table)))

(define (part-two x)
  (count-stones (blink-many 75 (list->table x))))

(display
 (part-two input))
(newline)
