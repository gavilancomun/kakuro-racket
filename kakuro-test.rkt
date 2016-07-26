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

(test-case "partby"
    (let* ([data (list 1 2 2 2 3 4 5 5 6 7 7 8 9)]
    [result (partition-by (lambda (n) (= 0 (remainder n 2))) data)])
    (printf "partby")
    (display result)
    (check-equal? 9 (length result))))

(test-case "partall"
    (let* ([data (list 1 2 2 2 3 4 5 5 6 7 7 8 9)]
    [result (partition-all 5 3 data)])
    (printf "partall")
    (display result)
    (check-equal? 5 (length result))))

(test-case "gather"
  (let* ([line (list (da 3 4) (v) (v) (d 4) (e) (a 4) (v) (v))]
        [result (gather-values line)])
    (printf "gather ")
    (display result)
    (check-equal? 4 (length result))
    (check-equal? (da 3 4) (first (first result)))
    (check-equal? (d 4) (first (first (rest (rest result)))))
    (check-equal? (e) (second (first (rest (rest result)))))
    (check-equal? (a 4) (first (rest (rest (first (rest (rest result)))))))))

(test-case "pair targets"
  (let* ([line (list (da 3 4) (v) (v) (d 4) (e) (a 4) (v) (v))]
        [result (pair-targets-with-values line)])
    (printf "pair ")
    (display result)
    (check-equal? 2 (length result))
    (check-equal? (da 3 4) (first (first (first result))))
    (check-equal? (d 4) (first (first (second result))))
    (check-equal? (e) (second (first (second result))))
    (check-equal? (a 4) (first (rest (rest (first (second result))))))))

(test-case "solve pair"
  (let* ([line (list (da 3 4) (v) (v) (d 4) (e) (a 4) (v) (v))]
        [pairs (pair-targets-with-values line)]
        [pair (first pairs)]
        [result (solve-pair down pair)])
    (printf "solvePair ")
    (display result)
    (check-equal? 3 (length result))
    (check-equal? (v 1 2) (second result))
    (check-equal? (v 1 2) (list-ref result 2))))

(test-case "solve line"
  (let* ([line (list (da 3 4) (v) (v) (d 4) (e) (a 5) (v) (v))]
         [result (solve-line line across)])
    (printf "solve line ")
    (display result)
    (check-equal? 8 (length result))
    (check-equal? (v 1 3) (second result))
    (check-equal? (v 1 3) (third result))
    (check-equal? (v 1 2 3 4) (seventh result))
    (check-equal? (v 1 2 3 4) (eighth result))))

(test-case "row"
  (let* ([result (solve-row (list (a 3) (v 1 2 3) (v 1)))])
    (printf "solve row ")
    (display result)
    (check-equal? (v 2) (second result))
    (check-equal? (v 1) (third result))))

(test-case "col"
    (let* ([result (solve-column (list (da 3 12) (v 1 2 3) (v 1)))])
    (print "solve col ")
    (display result)
    (check-equal? (v 2) (second result))
    (check-equal? (v 1) (third result))))
