#lang racket/base
(provide zone zone-update zone-add zone-add-bottom zone-remove zone-remove-bottom)
(require "method.rkt" racket/vector)

;; world of zones
;;   ;; zone-where-player-is
;;   key=>zone
;; potentially one-way links between zones

;(define (world)
;  (define key=>zone (hash))
;  (method-lambda
;    ))

(define (zone width height)
  (let zone ((grid (make-vector (* width height) '())))
    (define (position x y) (cons x y))
    (define (position-x pos) (car pos))
    (define (position-y pos) (cdr pos))
    (define (position->index pos) (+ (* (position-y pos) width) (position-x pos)))
    (method-lambda
      ((width)   width)
      ((height)  height)
      ((ref x y) (and (<= 0 x) (< x width) (<= 0 y) (< y height)
                   (vector-ref grid (position->index (position x y)))))
      ((set x y entities)
        (define grid.new (vector-copy grid))
        (vector-set! grid.new (position->index (position x y)) entities)
        (zone grid.new)))))

(define (zone-update z x y f)
  (define t (z 'ref x y))
  (and t (z 'set x y (f t))))
(define (zone-add z x y entity)
  (zone-update z x y (lambda (es) (cons entity es))))
(define (zone-add-bottom z x y entity)
  (zone-update z x y (lambda (es) (append es (list entity)))))
(define (zone-remove z x y entity)
  (zone-update z x y (lambda (es) (remove entity es))))
(define (zone-remove-bottom z x y entity)
  (zone-update z x y (lambda (es) (reverse (remove entity (reverse es))))))