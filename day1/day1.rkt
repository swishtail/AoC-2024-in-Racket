#lang racket
;;; Advent of Code - Day 1: Historian Hysteria
(define raw-input
  (file->lines "input"))

(define (make-left-list input)
  (define (select-left-element line)
    (string->number (car (string-split line))))
  (let construct-left-list ((remaining input))
    (if (null? remaining)
        '()
        (cons (select-left-element (car remaining))
              (construct-left-list (cdr remaining))))))

(define (make-right-list input)
  (define (select-right-element line)
    (string->number (cadr (string-split line))))
  (let construct-right-list ((remaining input))
    (if (null? remaining)
        '()
        (cons (select-right-element (car remaining))
              (construct-right-list (cdr remaining))))))

(define left-list (make-left-list raw-input))
(define right-list (make-right-list raw-input))

;;; Part One
(define (total-distance list1 list2)
  (define (differences list1 list2)
    (define (difference-of-pairs first second)
      (abs (- first second)))
    (map difference-of-pairs list1 list2))
  (define (sum-of-differences list-of-differences)
    (foldl + 0 list-of-differences))
  (sum-of-differences (differences (sort list1 <)
                                   (sort list2 <))))

(display
 (total-distance left-list
                 right-list))
(newline)

;;; Part Two
(define (similarity-score list1 list2)
  (define (run-length-encode-sorted-list sorted-list)
    (define (count-occurences number sorted-list)
      (let iter ((count 0)
                 (remaining sorted-list))
        (cond ((null? remaining)
               count)
              ((= number (car remaining))
               (iter (+ count 1)
                     (cdr remaining)))
              ((> number (car remaining))
               (iter count
                     (cdr remaining)))
              (else count))))
    (define (strip-first-block-of-repeaters sorted-list)
      (cond ((null? (cdr sorted-list))
             '())
            ((= (car sorted-list) (cadr sorted-list))
             (strip-first-block-of-repeaters (cdr sorted-list)))
            (else (cdr sorted-list))))
    (define (add-assoc-element key value assoc-list)
      (cons (list key value)
            assoc-list))
    (let build-assoc-list ((the-assoc-list '())
                           (remaining sorted-list))
      (if (null? remaining)
          (reverse the-assoc-list)
          (let ((current-number (car remaining)))
            (build-assoc-list
             (add-assoc-element current-number
                                (count-occurences current-number
                                                  remaining)
                                the-assoc-list)
             (strip-first-block-of-repeaters remaining))))))
  (define (sorted-assoc key sorted-assoc-list)
    (if (null? sorted-assoc-list)
        #f
        (let ((first-element (caar sorted-assoc-list)))
          (cond ((< key first-element)
                 #f)
                ((= key first-element)
                 (car sorted-assoc-list))
                (else (sorted-assoc key (cdr sorted-assoc-list)))))))
  (define (single-similarity-score record1 record2)
    (* (car record1)
       (* (cadr record1)
          (cadr record2))))
  (define (first-key assoc-list)
    (caar assoc-list))
  (define (similarity-scores-list sorted-assoc-list1 sorted-assoc-list2)
    (if (null? sorted-assoc-list1)
        '()
        (let ((record (sorted-assoc (first-key sorted-assoc-list1)
                                    sorted-assoc-list2)))
          (if (false? record)
              (similarity-scores-list (cdr sorted-assoc-list1)
                                      sorted-assoc-list2)
              (cons (single-similarity-score (car sorted-assoc-list1)
                                             record)
                    (similarity-scores-list (cdr sorted-assoc-list1)
                                            sorted-assoc-list2))))))
  (let ((sorted-list1 (sort list1 <))
        (sorted-list2 (sort list2 <)))
    (foldl + 0 (similarity-scores-list
                (run-length-encode-sorted-list sorted-list1)
                (run-length-encode-sorted-list sorted-list2)))))

(display
 (similarity-score left-list
                   right-list))
(newline)
