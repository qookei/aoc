(use-modules (srfi srfi-1) (srfi srfi-26)
             (ice-9 textual-ports) (ice-9 format)
             (ice-9 pretty-print) (ice-9 arrays))

(define (read-input)
  (let ([line (get-line (current-input-port))])
    (if (eof-object? line)
        '()
        (cons line (read-input)))))


(define (process-part-1 input)
  (array->list (transpose-array (list->array 2 (map string-tokenize input)) 1 0)))


(define (%process-part-2 input)
  (transpose-array (list->array 2 (map string->list input)) 1 0))

(define (process-part-2 input)
  (let ([out '()] [tmp '()])
    (array-slice-for-each-in-order
     1 (lambda (row)
         (let ([token (string-concatenate (string-tokenize (list->string (array->list row))))])
           (cond
            [(string=? "" token)
             (set! out (cons tmp out))
             (set! tmp '())]
            [(string-rindex token (char-set #\+ #\*))
             (set! tmp (cons (string-drop-right token 1)
                             (cons (string-take-right token 1) tmp)))]
            [else (set! tmp (cons token tmp))])))
     (%process-part-2 input))
    (cons tmp out)))


(define (part-common input)
  (apply
   + (map (lambda (row)
            (let* ([tokens (reverse row)]
                   [op-token (car tokens)]
                   [value-tokens (cdr tokens)])
              (apply (if (string=? "*" op-token) * +)
                     (map string->number value-tokens))))
          input)))


(let ([input (read-input)])
  (format #t "Part 1: ~a~%" (part-common (process-part-1 input)))
  (format #t "Part 2: ~a~%" (part-common (process-part-2 input))))
