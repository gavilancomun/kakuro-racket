#lang racket

(require racket/set)


(struct acrosscell (across))
(struct downcell (down))
(struct downacrosscell (down across))
(struct valuecell (values))
(struct emptycell ())

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

(provide (all-defined-out))
