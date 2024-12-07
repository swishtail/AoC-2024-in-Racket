#lang racket
;;; Advent of Code - Day 5: Print Queue
(define input
  (file->lines "input"))

(define (page-orders x)
  (if (string=? "" (car x))
      '()
      (cons (car x)
            (page-orders (cdr x)))))

(define (page-numbers x)
  (if (string=? "" (car x))
      (cdr x)
      (page-numbers (cdr x))))

(define (parse-page x)
  (map (λ (b) (map string->number b))
       (map (λ (a) (regexp-match* #rx"[0-9][0-9]" a))
            x)))

(define input-page-orders
  (parse-page (page-orders input)))

(define input-page-numbers
  (parse-page (page-numbers input)))

;;; Test input
(define test-input
  '("47|53"
  "97|13"
  "97|61"
  "97|47"
  "75|29"
  "61|13"
  "75|53"
  "29|13"
  "97|29"
  "53|29"
  "61|53"
  "97|53"
  "61|29"
  "47|13"
  "75|47"
  "97|75"
  "47|61"
  "75|61"
  "47|29"
  "75|13"
  "53|13"
  ""
  "75,47,61,53,29"
  "97,61,53,29,13"
  "75,29,13"
  "75,97,47,61,53"
  "61,13,29"
  "97,13,75,29,47"))

(define test-input-page-orders
  (parse-page (page-orders test-input)))

(define test-input-page-numbers
  (parse-page (page-numbers test-input)))

(define true-test
  (list-ref test-input-page-numbers 0))

(define false-test
  (list-ref test-input-page-numbers 3))

;;; Part One
(define (correctly-ordered-updates page-numbers page-orders)
  (define (special-assoc* key assoc-list)  ; This version of assoc* returns an
    (if (assoc key assoc-list)             ; empty list if no instances of key
        (let loop ((remaining assoc-list)) ; are found
          (if (null? remaining)
              '()
              (if (equal? key (caar remaining))
                  (cons (car remaining)
                        (loop (cdr remaining)))
                  (loop (cdr remaining)))))
        '()))
  (define (validate-list-of-page-numbers list-of-page-numbers
                                         list-of-page-orders)
    (define (test-first-number l list-of-page-orders)
      (define (list-second-element-of-sublists assoc-list)
        (if (null? assoc-list)
            '()
            (cons (cadar assoc-list)
                  (list-second-element-of-sublists (cdr assoc-list)))))
      (let* ((list-of-possible-next-pages
              (list-second-element-of-sublists (special-assoc*
                                                (car l)
                                                list-of-page-orders))))
        (let loop ((remaining (cdr l)))
          (if (null? remaining)
              #t
              (if (member (car remaining)
                          list-of-possible-next-pages)
                  (loop (cdr remaining))
                  #f)))))
    (if (null? list-of-page-numbers)
        #t
        (if (test-first-number list-of-page-numbers list-of-page-orders)
            (validate-list-of-page-numbers (cdr list-of-page-numbers)
                                           list-of-page-orders)
            #f)))
  (filter (lambda (a) (validate-list-of-page-numbers a page-orders))
          page-numbers))

(define (middle-of-page-numbers list-of-page-numbers)
  (list-ref list-of-page-numbers
            (floor (/ (length list-of-page-numbers)
                      2))))

(define (part-one page-numbers page-orders)
  (foldl +
         0
         (map middle-of-page-numbers
              (correctly-ordered-updates page-numbers
                                         page-orders))))

(display
 (part-one input-page-numbers
           input-page-orders))
(newline)
