#lang racket/base
(provide zone zone-width zone-height zone-ref zone-set
         zone-update zone-add zone-add-bottom zone-remove zone-remove-bottom
         world world-ref world-set world-zids world-find
         world-update world-add world-remove world-move
         (struct-out vec2) vec2+ vec2-scale dir->vec2
         (struct-out loc) loc-x loc-y)
(require racket/match racket/vector racket/list "method.rkt")

(struct vec2 (x y) #:prefab)
(define (vec2+ v.1 v.2)
  (vec2 (+ (vec2-x v.1) (vec2-x v.2)) (+ (vec2-y v.1) (vec2-y v.2))))
(define (vec2-scale v s)
  (vec2 (* s (vec2-x v)) (* s (vec2-y v))))

(struct loc (zid pos) #:prefab)
(define (loc-x loc) (vec2-x (loc-pos loc)))
(define (loc-y loc) (vec2-y (loc-pos loc)))

(define (dir->vec2 dir)
  (case dir
    ((up)    (vec2  0 -1))
    ((down)  (vec2  0  1))
    ((left)  (vec2 -1  0))
    ((right) (vec2  1  0))))

(define (world)
  (let world ((key=>zone (hash)))
    (method-lambda
      ((ref zone-id)  (hash-ref key=>zone zone-id)) ; TODO: what if zone-id is not in hash
      ((set zone-id zone) (world (hash-set key=>zone zone-id zone)))
      ((zone-ids) (hash-keys key=>zone))
      ((find-all entity) 
        (foldl 
          (lambda (kv acc) 
            (match-define `(,zone-id . ,zone) kv)
            (append 
              (map (lambda (xy) (loc zone-id xy)) 
                (zone-find-all zone entity))
              acc))
          '() 
          (hash->list key=>zone))))))

(define (world-ref  w zid)    (w 'ref zid))
(define (world-set  w zid z)  (w 'set zid z))
(define (world-zids w)        (w 'zone-ids))
(define (world-find w entity) (w 'find-all entity))

(define (world-update w l f)
  (world-set w (loc-zid l) (zone-update (world-ref w (loc-zid l)) (loc-x l) (loc-y l) f)))
(define (world-add w l entity)
  (world-set w (loc-zid l) (zone-add (world-ref w (loc-zid l)) (loc-x l) (loc-y l) entity)))
(define (world-remove w l entity)
  (world-set w (loc-zid l) (zone-remove (world-ref w (loc-zid l)) (loc-x l) (loc-y l) entity)))
(define (world-move w l.s l.t entity)
  (world-add (world-remove w l.s entity) l.t entity))

(module+ test 
  (require rackunit)
  (let* ((zone.one (zone 5 5)) 
         (zone.one (zone-set zone.one 1 1 '(pumpkin player)))
         (zone.one (zone-set zone.one 3 3 '(pumpkin)))
         (zone.two (zone-set zone.one 4 4 '(teleporter)))
         (w ((world) 'set 'zone.one zone.one))
         (w (w 'set 'zone.two zone.two))
         (players (w 'find-all 'player))
         (pumpkins (w 'find-all 'pumpkin))
         (teleporters (w 'find-all 'teleporter)))

    (check-not-false (member (loc zone.one (vec2 1 1)) players))
    (check-not-false (member (loc zone.two (vec2 1 1)) players))
    (check-false     (member (loc zone.one (vec2 4 4)) teleporters))
    (check-not-false (member (loc zone.two (vec2 4 4)) teleporters))
    (check-equal? (length pumpkins) 4)))

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

(define (zone-width  z) (z 'width))
(define (zone-height z) (z 'height))
(define (zone-ref z x y) (z 'ref x y))
(define (zone-set z x y entities) (z 'set x y entities))

(define (zone-update z x y f)
  (define tile (zone-ref z x y))
  (and tile (zone-set z x y (f tile))))
(define (zone-add z x y entity)
  (zone-update z x y (lambda (es) (cons entity es))))
(define (zone-add-bottom z x y entity)
  (zone-update z x y (lambda (es) (append es (list entity)))))
(define (zone-remove z x y entity)
  (zone-update z x y (lambda (es) (remove entity es))))
(define (zone-remove-bottom z x y entity)
  (zone-update z x y (lambda (es) (reverse (remove entity (reverse es))))))
(define (zone-find-all z entity)
  (filter-not not
    (map (lambda (xy)
          (and (member entity (apply zone-ref z xy)) (apply vec2 xy)))
      (cartesian-product (range (zone-width z)) (range (zone-height z))))))