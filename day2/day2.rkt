#lang racket
;;; Advent of Code - Day 2: Red-Nosed Reports
(define raw-input
  (file->lines "input"))

(define (map-sublists proc list-of-lists)
  (if (null? (cdr list-of-lists))
      (list (map string->number (car list-of-lists)))
      (cons (map string->number (car list-of-lists))
            (map-sublists proc (cdr list-of-lists)))))

(define data
  (map-sublists
   string->number
   (map string-split raw-input)))

;;; Part One
(define (safe-report? sequence)
  (define (map-two-at-a-time proc sequence)
    (let recur ((remaining sequence))
      (if (null? (cddr remaining))
          (list (proc (car remaining)
                      (cadr remaining)))
          (cons (proc (car remaining)
                      (cadr remaining))
                (recur (cdr remaining))))))
  (define (legal-differences? sequence)
    (define (legal-difference? n1 n2)
      (let ((difference (abs (- n2 n1))))
        (if (and (> difference 0)
                 (< difference 4))
            #t
            #f)))
    (let ((booleans (map-two-at-a-time legal-difference?
                                       sequence)))
      (if (memq #f booleans)
          #f
          #t)))
  (define (monotonic? sequence)
    (define (increasing? sequence)
      (define (positive-difference? n1 n2)
        (if (positive? (- n2 n1)) #t #f))
      (let ((booleans (map-two-at-a-time positive-difference?
                                         sequence)))
        (if (memq #f booleans)
            #f
            #t)))
    (define (decreasing? sequence)
      (define (negative-difference? n1 n2)
        (if (negative? (- n2 n1)) #t #f))
      (let ((booleans (map-two-at-a-time negative-difference?
                                         sequence)))
        (if (memq #f booleans)
            #f
            #t)))
    (if (or (increasing? sequence)
            (decreasing? sequence))
        #t
        #f))
  (if (and (legal-differences? sequence)
           (monotonic? sequence))
      #t
      #f))

(define (count-safe-reports boolean-list)
  (let iter ((counter 0)
             (remaining boolean-list))
    (if (null? remaining)
        counter
        (if (car remaining)
            (iter (+ counter 1)
                  (cdr remaining))
            (iter counter
                  (cdr remaining))))))

(define (total-safe input)
  (count-safe-reports (map safe-report? input)))

(display
 (total-safe data))
(newline)

;;; Part Two
(define (remove-element sequence n)
  (if (zero? n)
      (cdr sequence)
      (cons (car sequence)
            (remove-element (cdr sequence)
                            (- n 1)))))

(define (problem-damper report)
  (let ((report-length (length report)))
    (let iter ((n 0)
               (report-under-test report))
      (if (= n report-length)
          (safe-report? report-under-test)
          (if (safe-report? report-under-test)
              #t
              (iter (+ n 1)
                    (remove-element report n)))))))

(define (total-safe-with-damper data)
  (count-safe-reports (map problem-damper data)))

(display
 (total-safe-with-damper data))
(newline)
