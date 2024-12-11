#lang racket
(provide list2->zipper2
         zipper2->list2
         zipper2-select
         set-zipper2
         zipper2-up
         zipper2-down
         zipper2-left
         zipper2-right)

(define (list->zipper x k)
  (let iterate ((n k)
                (result '())
                (remaining x))
    (if (zero? n)
        (cons result
              (cons (car remaining)
                    (list (cdr remaining))))
        (iterate (- n 1)
                 (cons (car remaining)
                       result)
                 (cdr remaining)))))

(define (zipper->list z)
  (append (reverse (car z))
          (append (list (cadr z))
                  (caddr z))))

(define (zipper-select z)
  (cadr z))

(define (set-zipper z v)
  (list (car z)
        v
        (caddr z)))

(define (zipper-left z)
  (let ((a (car z))
        (b (cadr z))
        (c (caddr z)))
    (list (cdr a)
          (car a)
          (cons b c))))

(define (zipper-right z)
  (let ((a (car z))
        (b (cadr z))
        (c (caddr z)))
    (list (cons b a)
          (car c)
          (cdr c))))

(define (list2->zipper2 l row col)
  (list->zipper
   (map (lambda (a) (list->zipper a col))
        l)
   row))

(define (zipper2->list2 z)
  (map zipper->list
       (zipper->list z)))

(define (zipper2-select z)
  (zipper-select (cadr z)))

(define (set-zipper2 z v)
  (list (car z)
        (set-zipper (zipper-select z) v)
        (caddr z)))

(define (zipper2-up z)
  (zipper-left z))

(define (zipper2-down z)
  (zipper-right z))

(define (zipper2-left z)
  (let ((a (car z))
        (b (cadr z))
        (c (caddr z)))
    (list (map zipper-left a)
          (zipper-left b)
          (map zipper-left c))))

(define (zipper2-right z)
  (let ((a (car z))
        (b (cadr z))
        (c (caddr z)))
    (list (map zipper-right a)
          (zipper-right b)
          (map zipper-right c))))
