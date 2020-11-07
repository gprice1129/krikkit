#lang racket/base
(provide zone zone-width zone-height zone-ref zone-set
         zone-update zone-add zone-add-bottom zone-remove zone-remove-bottom
         world world-ref world-set world-zids world-find
         world-update world-add world-remove world-move
         (struct-out vec2) vec2+ vec2-scale dir->vec2
         (struct-out loc) loc-x loc-y)
(require racket/match racket/vector racket/list racket/pretty "method.rkt")

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
              (map (lambda (v) (loc zone-id v)) 
                (zone-find-all zone entity))
              acc))
          '() 
          (hash->list key=>zone))))))

(define (world-ref  w zid)    (w 'ref zid))
(define (world-set  w zid z)  (w 'set zid z))
(define (world-zids w)        (w 'zone-ids))
(define (world-find w entity) (w 'find-all entity))

(define (world-update w l f)
  (world-set w (loc-zid l) (zone-update (world-ref w (loc-zid l)) (loc-pos l) f)))
(define (world-add w l entity)
  (world-set w (loc-zid l) (zone-add (world-ref w (loc-zid l)) (loc-pos l) entity)))
(define (world-remove w l entity)
  (world-set w (loc-zid l) (zone-remove (world-ref w (loc-zid l)) (loc-pos l) entity)))
(define (world-move w l.s l.t entity)
  (world-add (world-remove w l.s entity) l.t entity))

(module+ test 
  (require rackunit)
  (let* ((zone.one (zone 5 5)) 
         (zone.one (zone-set zone.one (vec2 1 1) '(pumpkin player)))
         (zone.one (zone-set zone.one (vec2 3 3) '(pumpkin)))
         (zone.two (zone-set zone.one (vec2 4 4) '(teleporter)))
         (w ((world) 'set 'zone.one zone.one))
         (w (w 'set 'zone.two zone.two))
         (players (w 'find-all 'player))
         (pumpkins (w 'find-all 'pumpkin))
         (teleporters (w 'find-all 'teleporter)))

    (pretty-print (zone-ref zone.one (vec2 1 1)))
    (check-not-false (member (loc 'zone.one (vec2 1 1)) players))
    (check-not-false (member (loc 'zone.two (vec2 1 1)) players))
    (check-false     (member (loc 'zone.one (vec2 4 4)) teleporters))
    (check-not-false (member (loc 'zone.two (vec2 4 4)) teleporters))
    (check-equal? (length pumpkins) 4)))

(define (zone width height)
  (let zone ((grid (make-vector (* width height) '())))
    (define (vec2->zindex v) (+ (* (vec2-y v) width) (vec2-x v)))
    (method-lambda
      ((width)   width)
      ((height)  height)
      ((ref v)
        (define-values (x y) (values (vec2-x v) (vec2-y v)))
        (and (<= 0 x) (< x width) (<= 0 y) (< y height)
             (vector-ref grid (vec2->zindex v))))
      ((set v entities)
        (define grid.new (vector-copy grid))
        (vector-set! grid.new (vec2->zindex v) entities)
        (zone grid.new)))))

(define (zone-width  z) (z 'width))
(define (zone-height z) (z 'height))
(define (zone-ref z v) (z 'ref v))
(define (zone-set z v entities) (z 'set v entities))

(define (zone-update z v f)
  (define tile (zone-ref z v))
  (and tile (zone-set z v (f tile))))
(define (zone-add z v entity)
  (zone-update z v (lambda (es) (cons entity es))))
(define (zone-add-bottom z v entity)
  (zone-update z v (lambda (es) (append es (list entity)))))
(define (zone-remove z v entity)
  (zone-update z v (lambda (es) (remove entity es))))
(define (zone-remove-bottom z v entity)
  (zone-update z v (lambda (es) (reverse (remove entity (reverse es))))))
(define (zone-find-all z entity)
  (filter-not not
    (map (lambda (xy)
          (define v (apply vec2 xy)) 
          (and (member entity (zone-ref z v)) v))
      (cartesian-product (range (zone-width z)) (range (zone-height z))))))