#lang racket
;;; Advent of Code - Day 4: Ceres Search
(define input
  (file->lines "input"))

;;; Part One
(define (match-xmas str)
  (regexp-match* #rx"XMAS" str))

(define (string-reverse str)
  (list->string (reverse (string->list str))))

(define (transpose x)
  (apply map string (map string->list x)))

(define (shift-rows x)
  (let recurse ((number-of-rows (length x))
                (remaining x))
    (if (null? remaining)
        '()
        (cons (string-append (make-string (- number-of-rows 1)
                                          #\space)
                             (car remaining))
              (recurse (- number-of-rows 1)
                       (cdr remaining))))))

(define (fill-remaining x)
  (let recurse ((counter 0)
                (remaining x))
    (if (null? remaining)
        '()
        (cons (string-append (car remaining)
                             (make-string counter #\space))
              (recurse (+ counter 1)
                       (cdr remaining))))))

(define (horizontal-forwards  x) x)
(define (horizontal-backwards x) (map string-reverse x))
(define (vertical-downwards   x) (transpose x))
(define (vertical-upwards     x) (map string-reverse (transpose x)))

(define (diagonal-downwards-right x)
  (transpose (fill-remaining (shift-rows x))))
(define (diagonal-upwards-left x)
  (map string-reverse (diagonal-downwards-right x)))
(define (diagonal-downwards-left x)
  (diagonal-downwards-right (map string-reverse x)))
(define (diagonal-upwards-right x)
  (map string-reverse (diagonal-downwards-left x)))

(define (match-and-count-xmas x)
  (foldl +
         0
         (map length
              (map match-xmas
                   x))))

(define (count-all-xmas x)
  (+ (match-and-count-xmas (horizontal-forwards      x))
     (match-and-count-xmas (horizontal-backwards     x))
     (match-and-count-xmas (vertical-downwards       x))
     (match-and-count-xmas (vertical-upwards         x))
     (match-and-count-xmas (diagonal-downwards-right x))
     (match-and-count-xmas (diagonal-downwards-left  x))
     (match-and-count-xmas (diagonal-upwards-right   x))
     (match-and-count-xmas (diagonal-upwards-left    x))))

(define (part-one x)
  (count-all-xmas x))
(display
 (part-one input))
(newline)

;;; Part Two
(define (map-find-and-replace x)
  (map (λ (a) (regexp-replace* #rx"MAS"
                               a
                               "DRL"))
       x))

(define (MAS->DRL-diagonal-downwards-right x)
  (map string-trim
       (transpose
        (map-find-and-replace
         (diagonal-downwards-right x)))))

(define (MAS->DRL-diagonal-upwards-right x)
  (reverse
   (map string-trim
        (map string-reverse
             (transpose
              (map-find-and-replace
               (diagonal-upwards-right x)))))))

(define (MAS->DRL-diagonal-downwards-left x)
  (map string-reverse
       (map string-trim
            (transpose
             (map-find-and-replace
              (diagonal-downwards-left x))))))

(define (MAS->DRL-diagonal-upwards-left x)
  (reverse
   (map string-trim
        (transpose
         (map-find-and-replace
          (diagonal-upwards-left x))))))

(define (find-Rs x)
  (let ((list-of-charlists (map string->list x)))
    (map (lambda (b)
           (map (lambda (a) (eq? a #\R))
                b))
         list-of-charlists)))

(define (find-common-Rs a b)
  (define (all-true? a b)
    (and a b))
  (if (null? a)
      '()
      (cons (map all-true?
                 (car a)
                 (car b))
            (find-common-Rs (cdr a)
                            (cdr b)))))

(define (count-trues-in-list l)
  (let iterate ((counter 0)
                (remaining l))
    (if (null? remaining)
        counter
        (if (car remaining)
            (iterate (+ counter 1)
                     (cdr remaining))
            (iterate counter
                     (cdr remaining))))))

(define (count-all-trues x)
  (define (f l)
    (foldl +
           0
           (map count-trues-in-list l)))
  (+ (f (find-common-Rs (find-Rs (MAS->DRL-diagonal-downwards-right input))
                        (find-Rs (MAS->DRL-diagonal-downwards-left  input))))
     (f (find-common-Rs (find-Rs (MAS->DRL-diagonal-downwards-right input))
                        (find-Rs (MAS->DRL-diagonal-upwards-right   input))))
     (f (find-common-Rs (find-Rs (MAS->DRL-diagonal-upwards-right   input))
                        (find-Rs (MAS->DRL-diagonal-upwards-left    input))))
     (f (find-common-Rs (find-Rs (MAS->DRL-diagonal-upwards-left    input))
                        (find-Rs (MAS->DRL-diagonal-downwards-left  input))))))

(define (part-two x)
  (count-all-trues x))
(display
 (part-two input))
(newline)
