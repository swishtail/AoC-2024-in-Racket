#lang racket
;;; Advent of Code - Day 7: Bridge Repair
(define input
  (file->lines "input"))

(define (format-input x)
  (map (lambda (ls) (map string->number ls))
       (map (λ (a) (regexp-match* "[0-9]+" a)) x)))

;;; Part One
(define (one-armed-bandit places . symbols)
  (define (decimal->n-ary-list number base)
    (let iter ((n number) (r '()))
      (if (= n 0)
          r
          (iter (floor (/ n base))
                (cons (remainder n base)
                      r)))))
  (define (zeros n)
    (if (>= 0 n)
        '()
        (cons 0
              (zeros (- n 1)))))
  (define (prepend-zeros lst n)
    (let ((current-length (length lst)))
      (append (zeros (- n current-length))
              lst)))
  (define (switch-number-with-symbol-in-list l symbols)
    (map (λ (n) (list-ref symbols n)) l))
  (let* ((base (length symbols))
         (number (expt base places)))
    (map (λ (l) (switch-number-with-symbol-in-list l symbols))
         (map (λ (l) (prepend-zeros l places))
              (map (λ (n) (decimal->n-ary-list n base))
                   (range 0 number))))))

(define (fold-left-with-operators operators operands)
  (let iterate ((remaining-operators operators)
                (remaining-operands (cdr operands))
                (result (car operands)))
    (if (null? remaining-operands)
        result
        (iterate (cdr remaining-operators)
                 (cdr remaining-operands)
                 ((car remaining-operators) result
                                            (car remaining-operands))))))

(define (test-operands operands . operators)
  (define (test-one-permutation permutation)
    (= (car operands)
       (fold-left-with-operators permutation
                                 (cdr operands))))
  (let ((operator-permutations
         (apply one-armed-bandit (- (length operands) 2) operators)))
    (if (memq #t
              (map test-one-permutation operator-permutations))
        #t
        #f)))

(define (part-one x)
  (let ((formatted-input (format-input x)))
    (apply +
           (map car
                (filter (λ (x) (test-operands x + *))
                        formatted-input)))))

(display
 (part-one input))
(newline)
            
;;; Part Two
(define (|| a b)
  (let ((a-string (number->string a))
        (b-string (number->string b)))
    (string->number (string-append a-string b-string))))

(define (part-two x)
  (let ((formatted-input (format-input x)))
    (apply +
           (map car
                (filter (λ (x) (test-operands x + * ||))
                        formatted-input)))))

(display
 (part-two input))
(newline)
