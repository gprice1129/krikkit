#lang racket/base
(require pict pict/flash racket/draw racket/class racket/gui/base
         racket/list racket/match racket/pretty racket/vector
         "tile_world.rkt" "windows.rkt")

(define image (standard-fish 100 45 #:open-mouth #t #:color (make-color 128 0 128)))
(define image2 (jack-o-lantern 100 (make-color 128 0 128)))
(define joined-image (hc-append image2 image))

(define evq (event-queue))

(define ((event-pusher tag) event)
  (evq 'push (cons tag event)))

(define width 400)
(define height 450)

(define map-tile-types
  '#(() (wall) (candle) (pumpkin) (pumpkin candle) (fish) (fish candle) ((portal 1 1 3)) ((portal 0 2 2))))

(define map.w0.z0
  (list '(8 9)
    (map (lambda (type-index) (vector-ref map-tile-types type-index))
       '(0 0 1 1 1 1 1 0
         1 1 1 0 0 0 1 0
         1 2 5 3 0 0 1 0
         1 1 1 0 3 2 1 0
         1 2 1 1 3 0 1 0
         1 0 1 0 2 0 1 1
         1 3 0 4 3 3 2 1
         1 0 0 0 2 0 7 1
         1 1 1 1 1 1 1 1))))

(define map.w0.z1
  (list '(8 7)
    (map (lambda (type-index) (vector-ref map-tile-types type-index))
         '(1 1 1 1 1 1 1 1
           1 0 0 1 0 0 0 1
           1 0 3 2 2 3 0 1
           1 0 3 2 4 0 1 1
           1 0 3 2 2 3 0 1
           1 8 0 1 0 0 0 1
           1 1 1 1 1 1 1 1))))

(define map.w1 (list map.w0.z0 map.w0.z1))

(define (event->actions ev)
  (match ev
    (`(key down ,char)
      (match char
        (#\w '((move up)))
        (#\a '((move left)))
        (#\d '((move right)))
        (#\s '((move down)))
        (#\r '(restart))
        (_   '())))
    (_ '())))

(define (handle-action sw action)
  (or (match action
        (`(move ,direction) (sw 'move direction))
        ('restart           (sw 'restart))
        (_                  (error "unrecognized action:" action)))
      sw))

(define (handle-actions sw actions)
  (foldl (lambda (action sw) (handle-action sw action))
         sw
         actions))

(define (game-loop init-world world-update world-present!)
  (let loop ((sw init-world))
    (define sw.new (world-update sw))
    (world-present! sw.new)
    (sleep 0.01)
    (unless (sw.new 'winning?) (loop sw.new))))

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
  (let state ((w initial-world))
    (define loc.fish (car (world-find w 'fish)))
    (define z (world-ref w (loc-zid loc.fish)))
    (method-lambda
      ((current-zone) z)
      ((restart)      (state initial-world))
      ((winning?)
        (let loop ((x (- (zone-width z) 1)) (y (- (zone-height z) 1)))
          (cond ((< x 0)                     (loop (- (zone-width z) 1) (- y 1)))
                ((< y 0)                     #t)
                (else (define tile (zone-ref z (vec2 x y)))
                  (if (or (not (member 'pumpkin tile)) (member 'candle tile))
                    (loop (- x 1) y)
                    #f)))))
      ((move dir)
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

          (and w.new (state w.new))))))

(define (scale->entity->pict scale)
  (define fish (standard-fish scale (/ scale 2)))
  (define pumpkin (jack-o-lantern scale))
  (define wall (standard-cat scale (* scale 0.90)))
  (define candle (circle scale))
  (define portal (filled-flash scale scale))
  (lambda (entity)
   (case entity
    ((candle)  candle)
    ((pumpkin) pumpkin)
    ((wall)    wall)
    ((fish)    fish)
    (else      (case (car entity)
                 ((portal) portal))))))

(define (draw-zone dc zone scale scale->entity->pict)
  (define entity->pict (scale->entity->pict scale)) 
  (define-values (tile-width tile-height) (values scale scale))
  (for* ((x (in-range (zone 'width)))
         (y (in-range (zone 'height))))
    (define-values (px-x px-y) (values (* tile-width x) (* tile-height y)))
    (for ((entity (reverse (zone 'ref (vec2 x y)))))
      (draw-pict (entity->pict entity) dc px-x px-y))))

(define (draw-state dc st)
  (define scale 50)
  (draw-zone dc (st 'current-zone) scale scale->entity->pict))

(module+ test
  (require rackunit)
  (check-not-false ((sokoban-state '(((4 3)
                                      (() (wall) (wall) ()
                                       () (pumpkin candle) () ()
                                       (fish) () () ())))) 'winning?))

  ; check-equal?
  (check-false ((sokoban-state '(((4 3)
                                  (() (wall) (wall) ()
                                   () (pumpkin candle) () (pumpkin)
                                   (candle) (fish) () ())))) 'winning?))

  (check-not-false ((sokoban-state '(((2 1) ((fish) ())))) 'move 'right))
  (check-false     ((sokoban-state '(((1 2) ((fish) ())))) 'move 'right))
  (check-not-false ((sokoban-state '(((2 1) (() (fish))))) 'move 'left))
  (check-false     ((sokoban-state '(((1 2) (() (fish))))) 'move 'left))
  (check-not-false ((sokoban-state '(((1 2) (() (fish))))) 'move 'up))
  (check-false     ((sokoban-state '(((2 1) (() (fish))))) 'move 'up))
  (check-not-false ((sokoban-state '(((1 2) ((fish) ())))) 'move 'down))
  (check-false     ((sokoban-state '(((2 1) ((fish) ())))) 'move 'down))

  (check-false     ((sokoban-state '(((2 1) ((fish) (pumpkin)))))           'move 'right))
  (check-not-false ((sokoban-state '(((3 1) ((fish) (pumpkin) ()))))        'move 'right))
  (check-false     ((sokoban-state '(((3 1) ((fish) (pumpkin) (pumpkin))))) 'move 'right))
  (check-false     ((sokoban-state '(((3 1) ((fish) (pumpkin) (wall)))))    'move 'right))

  (check-false     ((sokoban-state '(((2 1) ((fish) (wall))))) 'move 'right))

  (define test-state (sokoban-state '(((2 1) ((fish) ())))))
  (check-not-false
    (let ((st.new (test-state 'move 'right)))
      (and st.new
           ;; TODO: use ref instead of show
           (equal? (list (zone-ref (st.new 'current-zone) (vec2 0 0))
                         (zone-ref (st.new 'current-zone) (vec2 1 0)))
                   '(() (fish)))))))

;; raco test fish_pumpkin.rkt

(module+ main
  (define w (window (event-pusher 'key) (event-pusher 'mouse) width height))
  (define actions '())
  (define (world-present! st)
    (w 'render (lambda (dc) (draw-state dc st))))

  (define (handle-events st)
    (foldl
      (lambda (event st)
        (define as (event->actions event))
        (set! actions (append (reverse as) actions))
        (handle-actions st as))
      st
      (evq 'pop)))
  
  (define (replay-action! st)
    (if (null? actions)
        st
        (let ()
          (define a (car actions))
          (set! actions (cdr actions))
          (sleep 0.1)
          (handle-action st a))))
  
  (w 'set-title! "testing")
  (w 'show)
  (w 'set-background 128 128 128)

  (void (thread (lambda ()
          (define st.init (sokoban-state map.w1))
          (game-loop st.init handle-events world-present!)
          (set! actions (reverse actions))
          (pretty-print `(you won! using these actions: ,actions))
          (game-loop st.init replay-action! world-present!)
          (pretty-print '(replay over hope you liked it!))))))