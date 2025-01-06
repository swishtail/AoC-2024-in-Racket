#lang racket
;;; Advent of Code - Day 8: Resonant Collinearity
(define input
  (file->lines "input"))

(define (format-input x)
  (map string->list x))

;;; Part One
(define (add-or-update-database frequency coordinates database)
  (define (add-to-record new-value record)
    (cons (car record)
          (cons new-value
                (cdr record))))
  (if (null? database)
      (cons (list frequency coordinates)
            database)
      (let ((record (car database)))
        (if (equal? frequency (car record))
            (if (member coordinates record)
                database
                (cons (add-to-record coordinates record)
                      (cdr database)))
            (cons record
                  (add-or-update-database frequency
                                          coordinates
                                          (cdr database)))))))

(define (traverse-list2-compile-database l)
  (let outer ((database-outer '())
              (y 0)
              (remaining-outer l))
    (if (null? remaining-outer)
        database-outer
        (let inner ((database-inner database-outer)
                    (x 0)
                    (remaining-inner (car remaining-outer)))
          (if (null? remaining-inner)
              (outer database-inner
                     (+ y 1)
                     (cdr remaining-outer))
              (if (char=? #\. (car remaining-inner))
                  (inner database-inner
                         (+ x 1)
                         (cdr remaining-inner))
                  (inner (add-or-update-database (car remaining-inner)
                                                 (list x y)
                                                 database-inner)
                         (+ x 1)
                         (cdr remaining-inner))))))))

(define (antinodes a b)
  (let ((ax (car a))
        (ay (cadr a))
        (bx (car b))
        (by (cadr b)))
    (list (list (- (* 2 ax) bx)
                (- (* 2 ay) by))
          (list (- (* 2 bx) ax)
                (- (* 2 by) ay)))))

(define (find-map-max-coordinates map)
  (list (- (length (car map)) 1)
        (- (length map) 1)))

(define (within-bounds? coordinate max-coordinates)
  (let ((x (car coordinate))
        (y (cadr coordinate))
        (x-max (car max-coordinates))
        (y-max (cadr max-coordinates)))
    (and (>= x 0)
         (>= y 0)
         (<= x x-max)
         (<= y y-max))))

(define (find-antinodes database-entry)
  (let outer ((coordinates-outer (cdr database-entry))
              (result-outer '()))
    (if (null? (cdr coordinates-outer))
        result-outer
        (let inner ((coordinates-inner (cdr coordinates-outer))
                    (result-inner result-outer))
          (if (null? coordinates-inner)
              (outer (cdr coordinates-outer)
                     result-inner)
              (inner (cdr coordinates-inner)
                     (let ((current-antinodes
                            (antinodes (car coordinates-outer)
                                       (car coordinates-inner))))
                       (cons (car current-antinodes)
                             (cons (cadr current-antinodes)
                                   result-inner)))))))))

(define (remove-duplicates sequence)
  (let iterate ((result '())
                (remaining sequence))
    (if (null? remaining)
        (reverse result)
        (if (member (car remaining)
                    result)
            (iterate result
                     (cdr remaining))
            (iterate (cons (car remaining)
                           result)
                     (cdr remaining))))))

(define (flatten-antinode-list antinode-list)
  (if (null? antinode-list)
      '()
      (append (car antinode-list)
              (flatten-antinode-list (cdr antinode-list)))))

(define (part-one x)
  (let* ((formatted-input (format-input x))
         (map-max-coordinates (find-map-max-coordinates formatted-input))
         (antenna-database (traverse-list2-compile-database formatted-input))
         (list-of-antinodes
          (filter (λ (x) (within-bounds? x map-max-coordinates))
                  (remove-duplicates
                   (flatten-antinode-list
                    (map find-antinodes
                         antenna-database))))))
    (length list-of-antinodes)))

(display
 (part-one input))
(newline)
