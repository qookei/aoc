(use-modules (srfi srfi-1) (srfi srfi-26) (srfi srfi-69)
             (ice-9 textual-ports) (ice-9 format)
             (ice-9 pretty-print) (ice-9 arrays))

(define (read-ingredients)
  (let ([line (get-line (current-input-port))])
    (if (eof-object? line)
        '()
        (cons (string->number line)
              (read-ingredients)))))

(define (read-ranges)
  (let ([line (get-line (current-input-port))])
    (if (string=? "" line)
        '()
        (cons (map string->number (string-split line #\-))
              (read-ranges)))))

(define (in-range? v range)
  (<= (car range) v (cadr range)))

(define (ingredient-in-any-range? ingredient ranges)
  (cond
   [(null? ranges) #f]
   [(in-range? ingredient (car ranges)) #t]
   [else (ingredient-in-any-range? ingredient (cdr ranges))]))

(define (cut-out-overlaps range ranges)
  (let next ([bottom (car range)]
             [top (cadr range)]
             [ranges ranges])
    (cond
     [(> bottom top) '(1 0)]
     [(null? ranges) (list bottom top)]
     ;; Ourselves
     [(equal? (list bottom top) (car ranges)) (next bottom top (cdr ranges))]
     ;; Overlaps
     [(in-range? bottom (car ranges)) (next (1+ (cadar ranges))   top ranges)]
     [(in-range? top (car ranges))    (next bottom (1- (caar ranges)) ranges)]
     ;; Disjoint
     [else (next bottom top (cdr ranges))])))

(define (remove-overlaps! ranges)
  (for-each
   (lambda (range)
     (let ([new (cut-out-overlaps range ranges)])
       (set-car! range (car new))
       (set-car! (cdr range) (cadr new))))
   ranges))


(define (part-1 ranges ingredients)
  (apply
   + (map
      (lambda (ingredient)
        (if (ingredient-in-any-range? ingredient ranges)
            1 0))
      ingredients)))

(define (part-2 ranges)
  (apply
   + (map
      (lambda (range)
        (- (cadr range) (car range) -1))
      ranges)))


(let* ([ranges (read-ranges)]
       [ingredients (read-ingredients)])
  (delete-duplicates! ranges)
  (remove-overlaps! ranges)
  (format #t "Part 1: ~a~%" (part-1 ranges ingredients))
  (format #t "Part 2: ~a~%" (part-2 ranges)))
