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
  (acrosscell a))

(define (e)
  (emptycell))

(define (v) 
  (valuecell (set 1 2 3 4 5 6 7 8 9)))

(define (pad2 n)
  (let ([s (number->string n)])
    (if (= 1 (length s))
      (string-append "0" s)
      s)))

(define (drawvalue vs v)
  (if (set-member? vs v)
    (number->string v)
    "."))

(define (drawvalues vs)
  (if (= 1 (length vs))
    (string-append "     " (first vs) "    ")
    (apply string-append (map (lambda (x) (drawvalue vs x)) vs))))

(define (draw cell)
  (match cell
    [(emptycell) "   -----  "]
    [(downcell d) (string-append "   " (pad2 d) "\\--  ") ]
    [(acrosscell a) (string-append "   --\\" (pad2 a) "  ")]
    [(downacrosscell d a) (string-append "   " (pad2 d) "\\" (pad2 a) "  ")]
    [(valuecell vs) (drawvalues vs)]))

(define (drawRow row)
  (apply string-append (map (lambda (cell) (draw cell)) row)))

(provide (all-defined-out))
