#lang racket

(provide list-display-newline
         make-list
         replace-element)

(define (list-display-newline x)
  (for-each (lambda (a) (begin (display a)
                               (newline)))
            x))

(define (make-list len . fill)
  (if (null? fill)
      (if (zero? len)
          '()
          (cons 0
                (make-list (- len 1))))
        (if (zero? len)
            '()
            (cons (car fill)
                  (make-list (- len 1)
                             (car fill))))))

(define (replace-element x k new-element)
  (if (zero? k)
      (cons new-element
            (cdr x))
      (cons (car x)
            (replace-element (cdr x)
                             (- k 1)
                             new-element))))
