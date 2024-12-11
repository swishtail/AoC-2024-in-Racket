#lang racket
;;; Advent of Code - Day 6: Guard Gallivant
(require "list.rkt"
         "zipper.rkt")

(define input
  (file->lines "input"))

(define test-input
  (file->lines "test-input"))

(define (format-input x)
  (map string->list x))

;; Part One
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
                (remaining (map find-^-in-list llc)))
    (if (null? remaining)
        #f
        (if (car remaining)
            (list row (car remaining))
            (iterate (+ row 1)
                     (cdr remaining))))))

(define (surround-with-true llc)
  (let* ((grid-width (length (car llc)))
         (grid-height (+ (length llc) 2))
         (horizontal-trues
          (list (make-list grid-width #t))))
    (map (λ (b) (append (list #t) b))
         (map (λ (a) (append a (list #t)))
              (append horizontal-trues
                      (append llc horizontal-trues))))))

(define (zip-up x)
  (let ((formatted-input (surround-with-true (format-input x))))
    (list2->zipper2 formatted-input
                    (car (find-start-row-col formatted-input))
                    (cadr (find-start-row-col formatted-input)))))

(define (llc->zipper2 llc)
  (list2->zipper2 llc
                  (car (find-start-row-col llc))
                  (cadr (find-start-row-col llc))))

(define (next-direction previous-direction)
  (cond ((eq? previous-direction zipper2-up)
         zipper2-right)
        ((eq? previous-direction zipper2-right)
         zipper2-down)
        ((eq? previous-direction zipper2-down)
         zipper2-left)
        (else zipper2-up)))

(define (walk-guard-count map-zipper)
  (let iterate ((current-direction zipper2-up)
                (current-zipper map-zipper)
                (step-count 0))
    (cond ((eq? (zipper2-select current-zipper) #t)
           step-count)
          ((or (eq? (zipper2-select (current-direction current-zipper)) #\#)
               (eq? (zipper2-select (current-direction current-zipper)) #\O))
           (let ((new-direction (next-direction current-direction)))
             (iterate new-direction
                      (new-direction (set-zipper2 current-zipper 'mark))
                      (if (eq? (zipper2-select current-zipper) 'mark)
                          step-count
                          (+ step-count 1)))))
          (else (iterate current-direction
                         (current-direction (set-zipper2 current-zipper 'mark))
                         (if (eq? (zipper2-select current-zipper) 'mark)
                             step-count
                             (+ step-count 1)))))))

(display
 (walk-guard-count (zip-up input)))
(newline)

;;; Part Two
(define (walk-guard-result map-zipper)
  (let iterate ((current-direction zipper2-up)
                (current-zipper map-zipper))
    (cond ((eq? (zipper2-select current-zipper) #t)
           current-zipper)
          ((or (eq? (zipper2-select (current-direction current-zipper)) #\#)
               (eq? (zipper2-select (current-direction current-zipper)) #\O))
           (let ((new-direction (next-direction current-direction)))
             (iterate new-direction
                      (new-direction (set-zipper2 current-zipper
                                                  'mark)))))
          (else (iterate current-direction
                         (current-direction (set-zipper2 current-zipper
                                                         'mark)))))))

(define (place-obstacle llc row col)
  (let ((new-map
         (replace-element llc
                          row
                          (replace-element (list-ref llc row)
                                           col
                                           #\O))))
    new-map))

(define (make-list-of-mark-coordinates llc)
  (define (mark? s)
    (eq? s 'mark))
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

(define (make-list-of-obstacle-maps original-map list-of-mark-coordinates)
  (define (replace-start-position-with-^ original-llc incorrect-llc)
    (let ((row (car (find-start-row-col original-llc)))
          (col (cadr (find-start-row-col original-llc))))
      (replace-element incorrect-llc
                       row
                       (replace-element (list-ref incorrect-llc
                                                  row)
                                        col
                                        #\^))))
  (define (map-replace-start-position-with-^ incorrect-llc)
    (replace-start-position-with-^ (zipper2->list2 original-map)
                                   incorrect-llc))
  (map map-replace-start-position-with-^
       (if (null? list-of-mark-coordinates)
           '()
           (cons (place-obstacle (zipper2->list2 original-map)
                                 (caar list-of-mark-coordinates)
                                 (cadar list-of-mark-coordinates))
                 (make-list-of-obstacle-maps original-map
                                             (cdr list-of-mark-coordinates))))))

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
    (cond ((eq? (zipper2-select current-zipper) #t)
           #f)
          ((eq? (zipper2-select current-zipper) (mark-and-direction
                                                 current-direction))
           #t)
          ((or (eq? (zipper2-select (current-direction current-zipper)) #\#)
               (eq? (zipper2-select (current-direction current-zipper)) #\O))
           (let ((new-direction (next-direction current-direction)))
             (iterate new-direction
                      (new-direction (set-zipper2
                                      current-zipper
                                      (mark-and-direction
                                       current-direction))))))
          (else (iterate current-direction
                         (current-direction (set-zipper2
                                             current-zipper
                                             (mark-and-direction
                                              current-direction))))))))

(define (count-trues lb)
  (if (null? lb)
      0
      (if (car lb)
          (+ 1 (count-trues (cdr lb)))
          (count-trues (cdr lb)))))

(define (part-two input)
  (let* ((original-map (zip-up input))
         (marked-map (walk-guard-result original-map))
         (list-of-mark-coordinates (make-list-of-mark-coordinates
                                    (zipper2->list2 marked-map)))
         (list-of-obstacle-maps-llc (make-list-of-obstacle-maps
                                     original-map
                                     list-of-mark-coordinates))
         (list-of-obstacle-maps-zipper
          (map (λ (a) (list2->zipper2 a
                                      (car
                                       (find-start-row-col
                                        (car list-of-obstacle-maps-llc)))
                                      (cadr
                                       (find-start-row-col
                                        (car list-of-obstacle-maps-llc)))))
               list-of-obstacle-maps-llc))
         (list-of-looping-maps (map guard-loops?
                                    list-of-obstacle-maps-zipper)))
    (count-trues list-of-looping-maps)))
                                                                