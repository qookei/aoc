(use-modules (srfi srfi-1) (ice-9 textual-ports) (ice-9 format) (ice-9 pretty-print)
             (srfi srfi-26) (srfi srfi-69))

(define (read-input)
  (let* ([line   (get-line (current-input-port))]
         [ranges (string-split line #\,)])
    (map (cut string-split <> #\-) ranges)))

(define (generate-subranges range)
  (let ([left-value  (string->number (car range))]
        [left-len    (string-length (car range))]
        [right-value (string->number (cadr range))]
        [right-len   (string-length (cadr range))])
    (let next ([out     '()]
               [cur-len left-len])
      (if (eq? cur-len (1+ right-len))
          out
          (let ([bottom (max left-value  (expt 10 (1- cur-len)))]
                [top    (min right-value (1- (expt 10 cur-len)))])
            (next (cons (list bottom top)
                        out)
                  (1+ cur-len)))))))

(define (split-ranges ranges)
  (append-map generate-subranges ranges))

(define (make-prefix-lengths-part-1 length)
  (if (eq? 0 (remainder length 2))
      (list (inexact->exact (truncate (/ length 2))))
      '()))

(define (make-prefix-lengths-part-2 length)
  (filter
   (lambda (p)
     (eq? 0 (remainder length p)))
   (iota (inexact->exact (truncate (/ length 2))) 1)))

(define (test-prefixes pfx-len digits top bottom ht)
  (let ([min  (expt 10 (1- pfx-len))]
        [max  (1- (expt 10 pfx-len))]
        [rept (inexact->exact (truncate (/ digits pfx-len)))])
    (apply
     +
     (map
      (lambda (pfx-value)
        (let* ([pfx      (number->string pfx-value)]
               [cand-str (string-concatenate (make-list rept pfx))]
               [cand     (string->number cand-str)])
          (if (and (<= bottom cand top)
                   (hash-table-ref/default ht cand #t))
              (begin
                (hash-table-set! ht cand #f)
                cand)
              0)))
      (iota max min)))))

(define (part-common ranges make-prefix-lengths)
  (let ([ht (make-hash-table)])
    (apply
     +
     (map
      (lambda (range)
        (let* ([bottom   (car range)]
               [top      (cadr range)]
               [digits   (string-length (number->string top))]
               [pfx-lens (make-prefix-lengths digits)])
          (apply
           +
           (map
            (lambda (pfx-len)
              (test-prefixes pfx-len digits top bottom ht))
            pfx-lens))))
      (split-ranges ranges)))))

(let ([ranges (read-input)])
  (format #t "Part 1: ~a~%" (part-common ranges make-prefix-lengths-part-1))
  (format #t "Part 2: ~a~%" (part-common ranges make-prefix-lengths-part-2)))
