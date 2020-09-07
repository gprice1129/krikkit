#lang racket/base
(provide
  widget-maker new-events event-pusher pop-events!
)

(require pict pict/flash racket/draw racket/class racket/gui/base racket/match racket/pretty
         "widget_maker.rkt" "method.rkt")

(define image (standard-fish 100 45 #:open-mouth #t #:color (make-color 128 0 128)))
(define image2 (jack-o-lantern 100 (make-color 128 0 128)))
(define joined-image (hc-append image2 image))

;; thread-safe event queue
(define new-events (box '()))
(define ((event-pusher tag) event)
  (define tagged-event (cons tag event))
  (let loop ()
    (define es (unbox new-events))
    (unless (box-cas! new-events es (cons tagged-event es))
      (loop))))
(define (pop-events!)
  (let loop ()
    (define es (unbox new-events))
    (if (box-cas! new-events es '())
      (reverse es)
      (loop))))

(define width 400)
(define height 300)

(define wmaker (widget-maker (event-pusher 'key) (event-pusher 'mouse)))
(define window (wmaker))
(window 'set-title! "testing")
(window 'resize width height)
(window 'show)

(define pos-x 0)
(define pos-y 0)

(define bm  (make-object bitmap% width height))
(define bdc (new bitmap-dc% (bitmap bm)))
(send bdc set-background (make-object color% 128 128 128))



(void (thread (lambda ()
        (let loop ()
          (for ((event (pop-events!)))
            (match event
              (`(mouse motion ,x ,y) (set! pos-x x) (set! pos-y y))
              (_ (pretty-print event))))
          (send bdc clear)
          (draw-pict joined-image bdc pos-x pos-y)
          (window 'paint (lambda (dc) (send dc draw-bitmap bm 0 0)))
          (sleep 0.01)
          (loop)))))
