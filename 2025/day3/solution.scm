(use-modules (srfi srfi-1) (ice-9 textual-ports) (ice-9 format) (ice-9 pretty-print) (srfi srfi-26))

(define (read-input)
  (let ([line (get-line (current-input-port))])
    (if (eof-object? line)
        '()
        (cons (string->list line)
              (read-input)))))

(define (find-index-of-largest lst)
  (let next ([lst lst]
             [idx 0]
             [best-idx 0]
             [best-chr #\nul])
    (if (null? lst)
        (cons best-idx best-chr)
        (next (cdr lst)
              (1+ idx)
              (if (char>? (car lst) best-chr)
                  idx
                  best-idx)
              (if (char>? (car lst) best-chr)
                  (car lst)
                  best-chr)))))

(define (solve-bank bank length)
  (let next ([remaining length]
             [rest bank]
             [out '()])
    (if (eq? 0 remaining)
        (string->number (list->string (reverse out)))
        (let* ([candidates (drop-right rest (1- remaining))]
               [idx,chr (find-index-of-largest candidates)])
          (next (1- remaining)
                (drop rest (1+ (car idx,chr)))
                (cons (cdr idx,chr) out))))))

(define (solve banks length)
  (apply + (map (cut solve-bank <> length) banks)))


(let ([input (read-input)])
  (format #t "Part 1: ~a~%" (solve input 2))
  (format #t "Part 2: ~a~%" (solve input 12)))
