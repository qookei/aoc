;; Excuse the bad code, it was written on my phone :^)
(use-modules (srfi srfi-1) (ice-9 textual-ports) (srfi srfi-26))
(use-modules (ice-9 pretty-print) (ice-9 rdelim))

(define +input+ (read-line))
(define +raw-ranges+ (string-split +input+ #\,))
(define +ranges+ (map (cut string-split <> #\-) +raw-ranges+))
(pretty-print +ranges+)

(define (generate-subranges l r)
  (let ([lv (string->number l)]
        [ln (string-length l)]
        [rv (string->number r)]
        [rn (string-length r)])

    (let next ([out '()]
               [cl ln])
      (if (eq? cl (1+ rn))
          out
          (let ([b (max lv (expt 10 (1- cl)))]
                [t (min rv (1- (expt 10 cl)))])
            (next (cons (list b t) out)
                  (1+ cl))
            )))))


(define +split-ranges+
  (append-map
   (lambda (e)
     (generate-subranges (car e) (cadr e)))
   +ranges+))

(define (maybe-bad-range? range)
  (eq? 0 (remainder (string-length (number->string (car range))) 2)))


(define (check-bad v)
  (let* ([s (number->string v)]
         [l (string-length s)])
    (if (string=? (string-take s (/ l 2)) (string-drop s (/ l 2)))
        v 0)))


(define (bruteforce-range-p1 range)
  (let next ([sum 0]
             [cur (car range)])
    (if (eq? cur (1+ (cadr range)))
        sum
        (next (+ sum (check-bad cur))
              (1+ cur)))))

(define +cand+ (filter maybe-bad-range? +split-ranges+))

(pretty-print (apply + (map bruteforce-range-p1 +cand+)))
