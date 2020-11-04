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


;; game winning condition
;;   all pumpkins must become 🎃

;; input response
;;   w,a,s,d to attempt moving fish in a direction

(define map-tile-types
  '#(() (wall) (candle) (pumpkin) (pumpkin candle) (fish) (fish candle)))

(define map.level1
  (map (lambda (type-index) (vector-ref map-tile-types type-index))
       '(0 0 1 1 1 1 1 0
         1 1 1 0 0 0 1 0
         1 2 5 3 0 0 1 0
         1 1 1 0 3 2 1 0
         1 2 1 1 3 0 1 0
         1 0 1 0 2 0 1 1
         1 3 0 4 3 3 2 1
         1 0 0 0 2 0 0 1
         1 1 1 1 1 1 1 1)))

;(define-syntax my-let
;  (syntax-rules ()
;    ((_ ((lhs rhs) ...) body ...)
;     ((lambda (lhs ...) body ...) rhs ...))
;    ((_ name ((lhs rhs) ...) body ...)
;      (letrec ((name (lambda (lhs ...) body ...)))
;        (name rhs ...)))))

;for ((a as) (b bs)) for* ((a as) (b bs)) 

;(define-syntax my-let*
;  (syntax-rules ()
;    ((_ () body ...) (begin body ...))
;    ((_ ((lhs rhs) (lhs-rest rhs-rest) ...) body ...)
;     (let ((lhs rhs))
;       (my-let* ((lhs-rest rhs-rest) ...) body ...)))))

;(my-let ((x 5))
;  (pretty-print `(my-let x: ,5)))
;; '(move ,direction) '(restart)

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

(define (sokoban-world width height map-data)
;  (define (world grid) ...)
;  (world (list->vector map-data))

;  (let ((x y)) z)
;  ==>
;  ((lambda (x) z) y)

  (define initial-zone
    (foldl
      (lambda (i tile z)
        (z 'set (remainder i width) (quotient i width) tile))
      (zone width height)
      (range (length map-data))
      map-data))
  (let world ((z initial-zone))
    (define (position x y) (cons x y))
    (define (position-x pos) (car pos))
    (define (position-y pos) (cdr pos))
    (define (position->index pos) (+ (* (position-y pos) width) (position-x pos)))
    (method-lambda
      ((width)   (z 'width))
      ((height)  (z 'height))
      ((ref x y) (z 'ref x y))
      ((restart) (world initial-zone))
      ((winning?)
        (let loop ((x (- (z 'width) 1)) (y (- (z 'height) 1)))
          (cond ((< x 0)                     (loop (- (z 'width) 1) (- y 1)))
                ((< y 0)                     #t)
                (else (define tile  (z 'ref x y))
                  (if (or (not (member 'pumpkin tile)) (member 'candle tile))
                    (loop (- x 1) y)
                    #f)))))
      ((move direction)
          (define (simplify pos)
            (define x (position-x pos))
            (define y (position-y pos))
            (define tile (or (z 'ref x y) '(wall)))
            (cond ((member 'wall tile) 'obstacle)
                  ((member 'pumpkin tile) 'pumpkin)
                  (else 'vacant)))

          (define (offset pos dir)
            (case dir
              ((up)    (position (position-x pos) (- (position-y pos) 1)))
              ((down)  (position (position-x pos) (+ (position-y pos) 1)))
              ((left)  (position (- (position-x pos) 1) (position-y pos)))
              ((right) (position (+ (position-x pos) 1) (position-y pos)))))

          (define (move-fish z pos dir)
            (define target (offset pos dir))
            (case (simplify target)
              ((obstacle) #f)
              ((pumpkin) (let ((z (move-pumpkin z target dir)))
                            (and z (move-entity z 'fish pos target))))
              ((vacant) (move-entity z 'fish pos target))))

          (define (move-pumpkin z pos dir)
            (define target (offset pos dir))
            (case (simplify target)
              ((obstacle pumpkin) #f)
              ((vacant) (move-entity z 'pumpkin pos target))))

          (define (move-entity z entity pos target)
            (let* ((z (zone-remove z (position-x pos)    (position-y pos)    entity))
                   (z (zone-add    z (position-x target) (position-y target) entity)))
              z))

          (define fish-pos
            (let loop ((x (- (z 'width) 1)) (y 0))
              (cond ((< x 0)                     (loop (- (z 'width) 1) (+ y 1)))
                    ((member 'fish (z 'ref x y)) (position x y))
                    (else                        (loop (- x 1) y)))))

          (define z.new (move-fish z fish-pos direction))
          (and z.new (world z.new))))))

(define (scale->entity->pict scale)
  (define fish (standard-fish scale (/ scale 2)))
  (define pumpkin (jack-o-lantern scale))
  (define wall (standard-cat scale (* scale 0.90)))
  (define candle (circle scale))
  (lambda (entity)
   (case entity
    ((candle)  candle)
    ((pumpkin) pumpkin)
    ((wall)    wall)
    ((fish)    fish))))

(define (draw-zone dc zone scale scale->entity->pict)
  (define entity->pict (scale->entity->pict scale)) 
  (define-values (tile-width tile-height) (values scale scale))
  (for* ((x (in-range (zone 'width)))
         (y (in-range (zone 'height))))
    (define-values (px-x px-y) (values (* tile-width x) (* tile-height y)))
    (for ((entity (reverse (zone 'ref x y))))
      (draw-pict (entity->pict entity) dc px-x px-y))))

(define (draw-world dc w)
  (define scale 50)
  (draw-zone dc w scale scale->entity->pict))
 
(module+ test
  (require rackunit)
  (check-not-false ((sokoban-world 4 3 '(() (wall) (wall) ()
                                         () (pumpkin candle) () ()
                                         () () () ())) 'winning?))

  ; check-equal?
  (check-false ((sokoban-world 4 3 '(() (wall) (wall) ()
                                     () (pumpkin candle) () (pumpkin)
                                     (candle) () () ())) 'winning?))          

  (check-not-false ((sokoban-world 2 1 '((fish) ())) 'move 'right))
  (check-false ((sokoban-world 1 2 '((fish) ())) 'move 'right))
  (check-not-false ((sokoban-world 2 1 '(() (fish))) 'move 'left))
  (check-false ((sokoban-world 1 2 '(() (fish))) 'move 'left))
  (check-not-false ((sokoban-world 1 2 '(() (fish))) 'move 'up))
  (check-false ((sokoban-world 2 1 '(() (fish))) 'move 'up))
  (check-not-false ((sokoban-world 1 2 '((fish) ())) 'move 'down))
  (check-false ((sokoban-world 2 1 '((fish) ())) 'move 'down))

  (check-false ((sokoban-world 2 1 '((fish) (pumpkin))) 'move 'right))
  (check-not-false ((sokoban-world 3 1 '((fish) (pumpkin) ())) 'move 'right))
  (check-false ((sokoban-world 3 1 '((fish) (pumpkin) (pumpkin))) 'move 'right))
  (check-false ((sokoban-world 3 1 '((fish) (pumpkin) (wall))) 'move 'right))

  (check-false ((sokoban-world 2 1 '((fish) (wall))) 'move 'right))

  (define test-world (sokoban-world 2 1 '((fish) ())))
  (check-not-false
    (let ((world.new (test-world 'move 'right)))
      (and world.new
           ;; TODO: use ref instead of show
           (equal? (list (world.new 'ref 0 0) (world.new 'ref 1 0))
                   '(() (fish)))))))

;; raco test fish_pumpkin.rkt

(module+ main
  (define pos-x 0)
  (define pos-y 0)
  (define w (window (event-pusher 'key) (event-pusher 'mouse) width height))
  (define actions '())
  (define (world-present! sw)
    (w 'render (lambda (dc) (draw-world dc sw))))

  (define (handle-events sw)
    (foldl
      (lambda (event sw)
        (define as (event->actions event))
        (set! actions (append (reverse as) actions))
        (handle-actions sw as))
      sw
      (evq 'pop)))
  
  (define (replay-action! sw)
    (if (null? actions)
        sw
        (let ()
          (define a (car actions))
          (set! actions (cdr actions))
          (sleep 0.1)
          (handle-action sw a))))
  
  (w 'set-title! "testing")
  ;(w 'resize width height)
  (w 'show)
  (w 'set-background 128 128 128)

  (void (thread (lambda ()
          (define sw.init (sokoban-world 8 9 map.level1))
          (game-loop sw.init handle-events world-present!)
          (set! actions (reverse actions))
          (pretty-print `(you won! using these actions: ,actions))
          (game-loop sw.init replay-action! world-present!)
          (pretty-print '(replay over hope you liked it!))))))