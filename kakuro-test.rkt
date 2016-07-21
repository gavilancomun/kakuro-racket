#lang racket

(require rackunit
         "kakuro.rkt")

(test-case
  "draw row"
  (let* ([line (list (da 3 4) (v) (v 1 2) (d 4) (e) (a 5) (v 4) (v 1))]
         [result (drawRow line)])
    (printf "drawrow")
    (printf result)
    (check-equal? "    3\\ 4   123456789 12.......    4\\--     -----     --\\ 5       4         1    \n" result)))

(test-case
  "permute"
  (let* ([vs (list (v) (v) (v))]
         [results (permute-all vs 6)])
    (display results)
    (check-equal? 10 (length results))
    (let ([diff (filter all-different results)])
      (check-equal? 6 (length diff)))))

