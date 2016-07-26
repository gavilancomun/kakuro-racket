#lang racket

(require racket/set)


(struct acrosscell (across) #:transparent)
(struct downcell (down) #:transparent)
(struct downacrosscell (down across) #:transparent)
(struct valuecell (values) #:transparent)
(struct emptycell () #:transparent)

(define (da down across)
  (downacrosscell down across))

(define (d down)
  (downcell down))

(define (a across)
  (acrosscell across))

(define (e)
  (emptycell))

(define (v . values) 
  (valuecell
    (if (= 0 (length values))
      (set 1 2 3 4 5 6 7 8 9)
      (apply set values))))

(define (pad2 n)
  (let ([s (number->string n)])
    (if (= 1 (string-length s))
      (string-append " " s)
      s)))

(define (drawvalue vs v)
  (if (set-member? vs v)
    (number->string v)
    "."))

(define (drawvalues vs)
  (if (= 1 (set-count vs))
    (string-append "     " (number->string (first (set->list vs))) "    ")
    (apply string-append " " (map (lambda (x) (drawvalue vs x)) (list 1 2 3 4 5 6 7 8 9)))))

(define (draw cell)
  (match cell
    [(emptycell) "   -----  "]
    [(downcell d) (string-append "   " (pad2 d) "\\--  ") ]
    [(acrosscell a) (string-append "   --\\" (pad2 a) "  ")]
    [(downacrosscell d a) (string-append "   " (pad2 d) "\\" (pad2 a) "  ")]
    [(valuecell vs) (drawvalues vs)]))

(define (draw-row row)
  (string-append (apply string-append (map (lambda (cell) (draw cell)) row)) "\n"))

(define (all-different nums)
  (= (length nums) (set-count (apply set nums))))

(define (conj coll item)
  (append coll (list item)))

(define (permute vs target so-far)
  (if (>= target 1)
    (if (= (length so-far) (- (length vs) 1))
      (list (conj so-far target))
      (let* ([t1 (list-ref vs (length so-far))]
             [t2 (set->list (valuecell-values t1))]
             [t3 (map (lambda (x) (permute vs (- target x) (conj so-far x))) t2)])
        (apply append t3)))
    (list)))

(define (permute-all vs total)
  (permute vs total (list)))

(define (transpose m)
  (apply (curry map list) m))

(define (is-possible? cell n)
  (set-member? (valuecell-values cell) n))

(define (solve-step cells total)
  (let* ([final (- (length cells) 1)]
         [perms1 (permute-all cells total)]
         [perms2 (filter (lambda (x) (is-possible? (last cells) (list-ref x final))) perms1)]
         [perms3 (filter all-different perms2)])
    (map (lambda (x) (apply v x)) (transpose perms3))))

(define (partition-by f coll)
  (if (= 0 (length coll))
    (list)
    (let* ([head (first coll)]
           [fx (f head)]
           [group (takef coll (lambda (y) (equal? fx (f y))))])
      (append (list group) (partition-by f (drop coll (length group)))))))

(define (partition-all n step coll)
  (if (= 0 (length coll))
    coll
    (append (list (take coll (min n (length coll)))) (partition-all n step (drop coll (min step (length coll)))))))

(define (gather-values line)
  (partition-by valuecell? line))

(define (pair-targets-with-values line)
  (partition-all 2 2 (gather-values line)))

(define (down x)
  (match x
         [(downcell d) d]
         [(downacrosscell d a) d]
         [_ 0]))

(define (across x)
  (match x
         [(acrosscell n) n]
         [(downacrosscell d a) a]
         [_ 0]))

(define (solve-pair f pair)
  (let* ([nvs (first pair)])
    (if (< (length pair) 2)
      nvs
      (let* ([vs (second pair)])
        (append nvs (solve-step vs (f (last nvs))))))))

(define (solve-line line f)
  (let ([pairs (pair-targets-with-values line)])
    (apply append (map (curry solve-pair f) pairs))))

(provide (all-defined-out))
