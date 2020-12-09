#lang racket/base
(provide sokoban-state)
(require racket/match racket/list
         "tile_world.rkt" "method.rkt")

(define (sokoban-state map-data)
  (define initial-zones
    (map (lambda (map.z)
          (match-define (list width height) (car map.z))
          (define tile-codes (cadr map.z))
          (foldl
            (lambda (i tile z)
              (zone-set z (vec2 (remainder i width) (quotient i width)) tile))
            (zone width height)
            (range (length tile-codes))
            tile-codes))
      map-data))
  (define initial-world
    (foldl
      (lambda (i z w) (world-set w i z))
      (world)
      (range (length initial-zones))
      initial-zones))
  (let state ((w initial-world) (history '()))
    (define loc.fish (car (world-find w 'fish)))
    (define z (world-ref w (loc-zid loc.fish)))
    (define (restart) (state initial-world '()))
    (define (move dir)
      (define (simplify pos)
        (define tile (or (zone-ref z pos) '(wall)))
        (define (portal? entity) (and (pair? entity) (eq? 'portal (car entity))))
        (cond ((member 'wall tile)    'obstacle)
              ((member 'pumpkin tile) 'pumpkin)
              ((memf portal? tile) => (lambda (entities) (car entities)))
              (else                   'vacant)))

      (define (offset pos dir)
        (vec2+ pos (dir->vec2 dir)))

      (define fish-pos (loc-pos loc.fish))
      (define fish-zid (loc-zid loc.fish))

      (define target.fish (offset fish-pos dir))
      (define (move-fish w) (world-move w (loc fish-zid fish-pos) (loc fish-zid target.fish) 'fish))
      (define w.new
        (match (simplify target.fish)
          ('obstacle #f)
          ('pumpkin
            (define target.pumpkin (offset target.fish dir))
            (case (simplify target.pumpkin)
              ((vacant)
                (move-fish
                  (world-move w (loc fish-zid target.fish) (loc fish-zid target.pumpkin) 'pumpkin)))
              (else #f)))
          ('vacant              (move-fish w))
          (`(portal ,zid ,x ,y) (world-move w (loc fish-zid fish-pos) (loc zid (vec2 x y)) 'fish))))

      (state (if w.new w.new w) history))

    (method-lambda
      ((current-zone) z)
      ((winning?)
        (let loop ((x (- (zone-width z) 1)) (y (- (zone-height z) 1)))
          (cond ((< x 0)                     (loop (- (zone-width z) 1) (- y 1)))
                ((< y 0)                     #t)
                (else (define tile (zone-ref z (vec2 x y)))
                  (if (or (not (member 'pumpkin tile)) (member 'candle tile))
                    (loop (- x 1) y)
                    #f)))))
      ((history-add actions)
        (state w (append (reverse actions) history)))
      ((history) (reverse history))
      ((handle-action action)
        (match action
              (`(move ,direction) (move direction))
              ('restart           (restart))
              (_                  (error "unrecognized action:" action)))))))