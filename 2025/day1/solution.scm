(use-modules (srfi srfi-1) (ice-9 textual-ports) (ice-9 format) (ice-9 pretty-print))

(define (parse-line line)
  (let ([sgn (string-take line 1)]
	[val (string-drop line 1)])
    (* (string->number val)
       (if (string=? sgn "L") -1 +1))))

(define (read-input)
  (let ([line (get-line (current-input-port))])
    (if (eof-object? line)
	'()
	(cons (parse-line line)
	      (read-input)))))

(define (found-0s-part-1 cur val)
  (if (eq? cur 0) 1 0))

(define (found-0s-part-2 cur val)
  (truncate
   (/ (if (< val 0)
	  (- (euclidean-remainder (- cur) 100) val)
	  (+ cur val))
    100)))

(define (part-common input count-found-0s)
  (let next ([input input]
	     [cur 50]
	     [found-0s 0])
    (if (null? input)
	found-0s
	(next (cdr input)
	      (euclidean-remainder (+ cur (car input)) 100)
	      (+ found-0s
		 (count-found-0s cur (car input)))))))


(let ([input (read-input)])
  (format #t "Part 1: ~a~%" (part-common input found-0s-part-1))
  (format #t "Part 2: ~a~%" (part-common input found-0s-part-2)))
