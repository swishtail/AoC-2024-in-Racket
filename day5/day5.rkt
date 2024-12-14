#lang racket
;;; Advent of Code - Day 5: Print Queue
(define input
  (file->lines "input"))

(define test-input
  (file->lines "test-input"))

(define (parse-page x)
  (map (λ (b) (map string->number b))
       (map (λ (a) (regexp-match* #rx"[0-9][0-9]" a))
            x)))

(define (parse-input-page-numbers x)
  (define (extract-page-numbers x)
    (if (string=? "" (car x))
        (cdr x)
        (extract-page-numbers (cdr x))))
  (parse-page (extract-page-numbers x)))

(define (parse-input-page-orders x)
  (define (extract-page-orders x)
    (if (string=? "" (car x))
        '()
        (cons (car x)
              (extract-page-orders (cdr x)))))
  (parse-page (extract-page-orders x)))

;;; Part One
(define (assoc* key assoc-list)
  (if (assoc key assoc-list)
      (let loop ((remaining assoc-list))
        (if (null? remaining)
            '()
            (if (equal? key (caar remaining))
                (cons (car remaining)
                      (loop (cdr remaining)))
                (loop (cdr remaining)))))
      '()))

(define (search value assoc-list)
  (if (null? assoc-list)
      #f
      (if (equal? value (cadar assoc-list))
          (car assoc-list)
          (search value (cdr assoc-list)))))

(define (legal-page-order? page-list page-orders)
  (define (all-false? lb)
    (if (null? lb)
        #t
        (if (car lb)
            #f
            (all-false? (cdr lb)))))
  (define (test-car page-list)
    (let* ((first (car page-list))
           (remaining (cdr page-list))
           (assoc-lists-of-remaining
            (map (λ (a) (assoc* a page-orders))
                 remaining)))
      (if (all-false?
           (map (λ (a) (search first a))
                assoc-lists-of-remaining))
          #t
          #f)))
  (let iterate ((remaining page-list))
    (if (null? remaining)
        #t
        (if (test-car remaining)
            (iterate (cdr remaining))
            #f))))

(define (middle-of-page-list page-list)
  (list-ref page-list
            (floor (/ (length page-list)
                      2))))

(define (correctly-ordered-page-lists page-numbers page-orders)
  (filter (λ (a) (legal-page-order? a page-orders))
          page-numbers))

(define (part-one page-numbers page-orders)
  (foldl +
         0
         (map middle-of-page-list
              (correctly-ordered-page-lists page-numbers
                                            page-orders))))

(display
 (part-one (parse-input-page-numbers input)
           (parse-input-page-orders input)))
(newline)

;;; Part Two
(define (incorrectly-ordered-page-lists page-numbers page-orders)
  (filter (λ (a) (not (legal-page-order? a page-orders)))
          page-numbers))

(define (correct-page-list page-list page-orders)
  (let test-list ((current-list page-list))
    (if (legal-page-order? current-list
                           page-orders)
        current-list
        (test-list
         (let recurse-inner ((remaining current-list))
           (if (null? (cdr remaining))
               remaining
               (if (legal-page-order? (take remaining 2)
                                      page-orders)
                   (cons (car remaining)
                         (recurse-inner (cdr remaining)))
                   (let ((first (car remaining))
                         (second (cadr remaining)))
                     (cons second
                           (recurse-inner (cons first
                                                (cddr remaining))))))))))))

(define (part-two incorrect-page-numbers page-orders)
  (foldl +
         0
         (map middle-of-page-list
              (map (λ (a) (correct-page-list a page-orders))
                   incorrect-page-numbers))))

(display
 (let ((page-orders (parse-input-page-orders input)))
   (part-two (incorrectly-ordered-page-lists
              (parse-input-page-numbers input)
              page-orders)
             page-orders)))
