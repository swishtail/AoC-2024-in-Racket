#lang racket
;;; Advent of Code - Day 3: Mull It Over
(define input
  (file->string "input"))

;;; Part One
(define (part-one x)
  (foldl + 0
         (map (λ (c) (apply * c))
              (map (λ (b) (map string->number b))
                   (map (λ (a) (regexp-match* #rx"[0-9]+" a))
                        (regexp-match* #rx"mul\\([0-9]+,[0-9]+\\)" x))))))
(display
 (part-one input))
(newline)

;;; Part Two
(define (find-dos x)
  (regexp-match-positions* #rx"do\\(\\)" x))

(define (find-donts x)
  (regexp-match-positions* #rx"don't\\(\\)" x))

(define (find-all-ranges start-positions end-positions)
  (let iter ((start-list (map car start-positions))
             (end-list (map cdr end-positions))
             (result '()))
    (if (null? start-list)
        (reverse result)
        (if (< (car end-list)
               (car start-list))
            (iter start-list
                  (cdr end-list)
                  result)
            (iter (cdr start-list)
                  end-list
                  (cons (cons (car start-list)
                              (car end-list))
                        result))))))

(define (remove-redundant-ranges range-list)
  (let iter ((current-do-index 0)
             (remaining range-list)
             (result '()))
    (if (null? remaining)
        (reverse result)
        (if (> (cdar remaining)
               current-do-index)
            (iter (cdar remaining)
                  (cdr remaining)
                  (cons (car remaining)
                        result))
            (iter current-do-index
                  (cdr remaining)
                  result)))))

(define (pair->list pair)
  (list (car pair)
        (cdr pair)))

(define (invalid-ranges start-positions end-positions)
  (map pair->list
       (remove-redundant-ranges
        (find-all-ranges start-positions
                         end-positions))))

(define (extract-substrings-from-string str list-of-ranges)
  (map (λ (c)
         (apply (λ (a b)
                  (substring str a b)) c))
       list-of-ranges))

(define (remove-invalid-substrings str list-of-strings-to-remove)
  (let iter ((remaining list-of-strings-to-remove)
             (final-string str))
    (if (null? remaining)
        final-string
        (iter (cdr remaining)
              (string-replace final-string (car remaining)
                              "")))))

(define (part-two x)
  (part-one
   (remove-invalid-substrings
    x
    (extract-substrings-from-string
     x
     (invalid-ranges
      (find-donts x)
      (find-dos x))))))

(display
 (part-two input))
(newline)
