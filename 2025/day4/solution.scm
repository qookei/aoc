(use-modules (srfi srfi-1) (srfi srfi-26)
             (ice-9 textual-ports) (ice-9 format)
             (ice-9 pretty-print) (ice-9 arrays))

(define (%read-input)
  (let ([line (get-line (current-input-port))])
    (if (eof-object? line)
        '()
        (cons (map (cut char=? <> #\@) (string->list line))
              (%read-input)))))

(define (widen-bounds bounds)
  (list (1- (car bounds))
        (1+ (cadr bounds))))

(define (read-input)
  (let* ([in-array (list->array 2 (%read-input))]
         [in-shape (array-shape in-array)]
         [out-shape (map widen-bounds in-shape)]
         [out-array (apply make-array #f out-shape)])
    (array-copy! in-array
                 (apply
                  make-shared-array
                  out-array
                  list
                  in-shape))
    out-array))


(define (neighbors-of array x y)
  (make-shared-array
   array
   (lambda (dy dx)
     (list (+ dy y -1)
           (+ dx x -1)))
   3 3))

(define (is-center-accessible? array)
  (if (array-ref array 1 1)
      (let ([count 0])
        (array-for-each
         (lambda (v)
           (if v (set! count (1+ count))))
         array)
        (< (1- count) 4))
      #f))

(define (array-for-each-index proc array)
  (let ([shape (array-shape array)])
    (let next ([x 0]
               [y 0])
      (cond
       [(eq? y (cadar shape)) #f]
       [(eq? x (cadadr shape)) (next 0 (1+ y))]
       [else
        (begin
          (proc y x)
          (next (1+ x) y))]))))


(define (part-1 array)
  (let ([count 0])
    (array-for-each-index
     (lambda (y x)
       (if (is-center-accessible? (neighbors-of array x y))
           (set! count (1+ count))))
     array)
    count))

(define (part-2 array)
  (let ([count 0]
        [new-array (array-copy array)])
    (array-for-each-index
     (lambda (y x)
       (if (is-center-accessible? (neighbors-of array x y))
           (begin
             (set! count (1+ count))
             (array-cell-set! (neighbors-of new-array x y) #f 1 1))))
     array)
    (if (> count 0)
        (+ count (part-2 new-array))
        count)))


(let ([input (read-input)])
  (format #t "Part 1: ~a~%" (part-1 input))
  (format #t "Part 2: ~a~%" (part-2 input)))
