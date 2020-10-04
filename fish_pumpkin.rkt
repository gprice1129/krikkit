#lang racket/base
(require pict pict/flash racket/draw racket/class racket/gui/base
         racket/match racket/pretty racket/vector
         "windows.rkt")

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

(define (world width height map-data)
  (let world ((grid (list->vector map-data)))
    (define (position x y) (cons x y))
    (define (position-x pos) (car pos))
    (define (position-y pos) (cdr pos))
    (define (position->index pos) (+ (* (position-y pos) width) (position-x pos)))
    (method-lambda
      ((width)   width)
      ((height)  height)
      ((ref x y) (vector-ref grid (position->index (position x y))))
      ((restart)
        (let ((grid (vector-copy grid)))
          (let loop ((i 0) (data map-data))
            (unless (null? data)
              (vector-set! grid i (car data))
              (loop (+ i 1) (cdr data))))
          (world grid)))
      ((winning?)
        (for/and ((tile (in-vector grid)))
          ;; TODONT: collapse to single iteration check
          (or (not (member 'pumpkin tile)) (member 'candle tile))))
      ((move direction)
        (let ((grid (vector-copy grid)))
          (define (index->position index) (position (remainder index width)
                                                    (quotient index width)))

          (define (simplify pos)
            (define x (position-x pos))
            (define y (position-y pos))
            (define tile (if (or (< x 0) (< y 0) (>= x width) (>= y height))
                            '(wall)
                            (vector-ref grid (position->index pos))))
            (cond ((member 'wall tile) 'obstacle)
                  ((member 'pumpkin tile) 'pumpkin)
                  (else 'vacant)))

          (define (offset pos dir)
            (case dir
              ((up)    (position (position-x pos) (- (position-y pos) 1)))
              ((down)  (position (position-x pos) (+ (position-y pos) 1)))
              ((left)  (position (- (position-x pos) 1) (position-y pos)))
              ((right) (position (+ (position-x pos) 1) (position-y pos)))))

          (define (move-fish pos dir)
            (define target (offset pos dir))
            (case (simplify target)
              ((obstacle) #f)
              ((pumpkin) (and (move-pumpkin target dir)
                              (move-entity 'fish pos target)))
              ((vacant) (move-entity 'fish pos target))))

          (define (move-pumpkin pos dir)
            (define target (offset pos dir))
            (case (simplify target)
              ((obstacle pumpkin) #f)
              ((vacant) (move-entity 'pumpkin pos target))))

          (define (move-entity entity pos target)
            (define entity-index (position->index pos))
            (define target-index (position->index target))
            (define tile (vector-ref grid entity-index))
            (define target-tile (vector-ref grid target-index))
            (vector-set! grid entity-index (remq entity tile))
            (vector-set! grid target-index (cons entity target-tile))
            #t)

          (define fish-pos 
            (let loop ((i 0))
              (if (member 'fish (vector-ref grid i))
                  (index->position i)
                  (begin
                    (loop (+ i 1))))))

          (and (move-fish fish-pos direction)
               (world grid)))))))

(define (draw-world dc w)
  (define-values (tile-width tile-height) (values 50 50))
  (define fish (standard-fish 50 25))
  (define pumpkin (jack-o-lantern 50))
  (define wall (standard-cat 50 45))
  (define candle (circle 50))
  (for* ((x (in-range (w 'width)))
         (y (in-range (w 'height))))
    (define-values (px-x px-y) (values (* tile-width x) (* tile-height y)))
    (for ((entity (reverse (w 'ref x y))))
      (draw-pict
        (case entity
          ((candle)  candle)
          ((pumpkin) pumpkin)
          ((wall)    wall)
          ((fish)    fish))
        dc px-x px-y))))

(module+ test
  (require rackunit)
  (check-not-false ((world 4 3 '(() (wall) (wall) ()
               () (pumpkin candle) () () ()
               () () () ())) 'winning?))

  ;; check-equal?
  (check-false ((world 4 3 '(() (wall) (wall) ()
             () (pumpkin candle) () (pumpkin) (candle)
             () () () ())) 'winning?))          

  (check-not-false ((world 2 1 '((fish) ())) 'move 'right))
  (check-false ((world 1 2 '((fish) ())) 'move 'right))
  (check-not-false ((world 2 1 '(() (fish))) 'move 'left))
  (check-false ((world 1 2 '(() (fish))) 'move 'left))
  (check-not-false ((world 1 2 '(() (fish))) 'move 'up))
  (check-false ((world 2 1 '(() (fish))) 'move 'up))
  (check-not-false ((world 1 2 '((fish) ())) 'move 'down))
  (check-false ((world 2 1 '((fish) ())) 'move 'down))

  (check-false ((world 2 1 '((fish) (pumpkin))) 'move 'right))
  (check-not-false ((world 3 1 '((fish) (pumpkin) ())) 'move 'right))
  (check-false ((world 3 1 '((fish) (pumpkin) (pumpkin))) 'move 'right))
  (check-false ((world 3 1 '((fish) (pumpkin) (wall))) 'move 'right))

  (check-false ((world 2 1 '((fish) (wall))) 'move 'right))

  (define test-world (world 2 1 '((fish) ())))
  (check-not-false
    (let ((world.new (test-world 'move 'right)))
      (and world.new
           ;; TODO: use ref instead of show
           (equal? (list (world.new 'ref 0 0) (world.new 'ref 1 0))
                   '(() (fish)))))))

;(module+ your-own-whatever
;  (provide stuff)
;)

;; raco test fish_pumpkin.rkt

(module+ main
  (define pos-x 0)
  (define pos-y 0)
  (define w (window (event-pusher 'key) (event-pusher 'mouse) width height))
  (w 'set-title! "testing")
  (w 'resize width height)
  (w 'show)
  (w 'set-background 128 128 128)
  (void (thread (lambda ()
          (let loop ((sw (world 8 9 map.level1)))
            (define sw.new
              (foldl
                (lambda (event sw)
                  (match event
                    (`(key down ,char)
                      (define sw.new
                        (match char
                          (#\w (sw 'move 'up))
                          (#\a (sw 'move 'left))
                          (#\d (sw 'move 'right))
                          (#\s (sw 'move 'down))
                          (_ sw)))
                      (or sw.new sw))
                    (_ sw)))
                sw
                (evq 'pop)))
            (w 'render
              (lambda (dc)
                (draw-world dc sw.new)))
            (sleep 0.01)
            (loop (if (sw.new 'winning?) (sw.new 'restart) sw.new)))))))