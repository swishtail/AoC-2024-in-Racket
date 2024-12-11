#lang racket
;;; Advent of Code - Day 6: Guard Gallivant
(require "list.rkt"
         "zipper.rkt")

(define input
  (file->lines "input"))

;;; Part One
(define (x->llc x)
  (map string->list x))

(define (llc->zipper2 llc row col)
  (list2->zipper2 llc row col))

(define (pad-with-trues llc)
  (let* ((grid-width (length (car llc)))
         (grid-height (+ (length llc) 2))
         (horizontal-trues
          (list (make-list grid-width #t)))
         (list-true (list #t)))
    (map (λ (b) (append list-true b))
         (map (λ (a) (append a list-true))
              (append horizontal-trues
                      (append llc
                              horizontal-trues))))))

(define (find-start-row-col llc)
  (define (find-^-in-list lc)
    (let iterate ((col 0)
                  (remaining lc))
      (if (null? remaining)
          #f
          (if (eq? (car remaining)
                   #\^)
              col
              (iterate (+ col 1)
                       (cdr remaining))))))
  (let iterate ((row 0)
                (remaining (map find-^-in-list
                                llc)))
    (if (null? remaining)
        #f
        (if (car remaining)
            (list row (car remaining))
            (iterate (+ row 1)
                     (cdr remaining))))))

(define (next-direction previous-direction)
  (cond ((eq? previous-direction zipper2-up)
         zipper2-right)
        ((eq? previous-direction zipper2-right)
         zipper2-down)
        ((eq? previous-direction zipper2-down)
         zipper2-left)
        (else zipper2-up)))

(define (walk-guard-mark map-zipper)
  (let iterate ((current-direction zipper2-up)
                (current-zipper map-zipper))
    (if (eq? (zipper2-select current-zipper) #t)
        current-zipper
        (let ((next-position
           (zipper2-select
            (current-direction current-zipper))))
          (cond ((or (eq? next-position #\#)
                     (eq? next-position #\O))
                 (let ((new-direction
                        (next-direction current-direction)))
                   (iterate new-direction
                            current-zipper)))
                (else (iterate current-direction
                               (current-direction
                                (set-zipper2 current-zipper
                                             'mark)))))))))

(define (make-list-of-mark-coordinates llc)
  (define (mark? sym)
    (eq? sym 'mark))
  (let outer ((row-counter 0)
              (list-of-coordinates-outer '())
              (remaining-outer llc))
    (if (null? remaining-outer)
        list-of-coordinates-outer
        (let inner ((col-counter 0)
                    (list-of-coordinates-inner
                     list-of-coordinates-outer)
                    (remaining-inner (car remaining-outer)))
          (if (null? remaining-inner)
              (outer (+ row-counter 1)
                     list-of-coordinates-inner
                     (cdr remaining-outer))
              (if (mark? (car remaining-inner))
                  (inner (+ col-counter 1)
                         (cons (list row-counter col-counter)
                               list-of-coordinates-inner)
                         (cdr remaining-inner))
                  (inner (+ col-counter 1)
                         list-of-coordinates-inner
                         (cdr remaining-inner))))))))

(define (count-marks ll)
  (length ll))

(define (part-one x)
  (let* ((input-llc (x->llc x))
         (padded-input-llc (pad-with-trues input-llc))
         (start-coordinates
          (find-start-row-col padded-input-llc))
         (map-zipper
          (llc->zipper2 padded-input-llc
                        (car start-coordinates)
                        (cadr start-coordinates)))
         (marked-map (walk-guard-mark map-zipper))
         (list-of-mark-coordinates
          (make-list-of-mark-coordinates
           (zipper2->list2 marked-map))))
    (count-marks list-of-mark-coordinates)))

(display
 (part-one input))
(newline)

;;; Part Two
(define (remove-start-coordinates list-of-mark-coordinates
                                  start-coordinates)
  (remove start-coordinates list-of-mark-coordinates))

(define (place-obstacle llc mark-coordinates)
  (let recurse ((remaining llc)
                (row (car mark-coordinates))
                (col (cadr mark-coordinates)))
    (if (zero? row)
        (cons (replace-element (car remaining)
                               col
                               #\O)
              (cdr remaining))
        (cons (car remaining)
              (recurse (cdr remaining)
                       (- row 1)
                       col)))))

(define (mark-and-direction current-direction)
  (cond ((eq? current-direction zipper2-up)
         'mark-up)
        ((eq? current-direction zipper2-right)
         'mark-right)
        ((eq? current-direction zipper2-down)
         'mark-down)
        ((eq? current-direction zipper2-left)
         'mark-left)))

(define (guard-loops? map-zipper)
  (let iterate ((current-direction zipper2-up)
                (current-zipper map-zipper))
    (if (eq? (zipper2-select current-zipper) #t)
        #f
        (if (eq? (zipper2-select current-zipper)
                 (mark-and-direction current-direction))
            #t
            (let ((next-position
                   (zipper2-select
                    (current-direction current-zipper))))
              (if (or (eq? next-position #\#)
                      (eq? next-position #\O))
                  (let ((new-direction
                         (next-direction current-direction)))
                    (iterate new-direction
                             current-zipper))
                  (iterate current-direction
                           (current-direction
                            (set-zipper2
                             current-zipper
                             (mark-and-direction
                              current-direction))))))))))

(define (count-trues lb)
  (if (null? lb)
      0
      (if (car lb)
          (+ 1 (count-trues (cdr lb)))
          (count-trues (cdr lb)))))

(define (part-two x)
  (let* ((input-llc (x->llc x))
         (padded-input-llc (pad-with-trues input-llc))
         (start-coordinates
          (find-start-row-col padded-input-llc))
         (map-zipper
          (llc->zipper2 padded-input-llc
                        (car start-coordinates)
                        (cadr start-coordinates)))
         (marked-map (walk-guard-mark map-zipper))
         (list-of-mark-coordinates
          (make-list-of-mark-coordinates
           (zipper2->list2 marked-map)))
         (list-of-mark-coordinates-sans-start
          (remove-start-coordinates list-of-mark-coordinates
                                    start-coordinates))
         (obstacle-map-llcs
          (map (λ (a) (place-obstacle padded-input-llc
                                      a))
               list-of-mark-coordinates-sans-start))
         (obstacle-map-zippers
          (map (λ (b) (llc->zipper2 b
                                    (car start-coordinates)
                                    (cadr start-coordinates)))
               obstacle-map-llcs))
         (list-of-bools
          (map guard-loops? obstacle-map-zippers)))
    (count-trues list-of-bools)))

(display
 (part-two input))
(newline)
