#lang racket

(require rackunit
         "kakuro.rkt")

(test-case
  "draw row"
  (let* ([line (list (da 3 4) (v) (v 1 2) (d 4) (e) (a 5) (v 4) (v 1))]
         [result (draw-row line)])
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

(test-case
  "transpose"
  (let* ([ints '((1 2 3 4) (1 2 3 4) (1 2 3 4))]
         [tr (transpose ints)])
    (display ints)
    (display tr)
    (check-equal? (length ints) (length (first tr)))
    (check-equal? (length (first ints)) (length tr))))

(test-case
  "lists"
  (let* ([a '(1 2 3)]
         [b '(4 5 6 1 2 3)])
    (check-equal? 4 (length (takef '(0 1 2 3 4 5 6 7 8 9) (lambda (n) (< n 4)))))
    (check-equal? 9 (length (append a b)))
    (check-equal? 2 (length (drop b 4)))
    (check-equal? 4 (length (take b 4)))))

(test-case "isposs"
           (let* ([vc (v 1 2 3)])
             (check-equal? true (is-possible? vc 2))
             (check-equal? false (is-possible? vc 4))))

(test-case "solvestep"
           (let ([result (solve-step (list (v 1 2) (v)) 5)])
             (printf "solve step result ")
             (display result)
             (check-equal? (v 1 2) (first result))
             (check-equal? (v 3 4) (second result))))

