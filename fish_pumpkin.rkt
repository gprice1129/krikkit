#lang racket/base
(provide
  widget-maker new-events event-pusher pop-events!
)

(require pict pict/flash racket/draw racket/class racket/gui/base racket/match racket/pretty
         "method.rkt")

(define image (standard-fish 100 45 #:open-mouth #t #:color (make-color 128 0 128)))
(define image2 (jack-o-lantern 100 (make-color 128 0 128)))
(define joined-image (hc-append image2 image))

(define (widget-maker on-key on-mouse)
  (define widget-frame%
    frame%
    ;(class frame%)
    )
  (define widget-canvas%
    (class canvas%
      (init) (super-new)
      (define/override (on-event event)
        (on-mouse
          (case (send event get-event-type)
            ((enter leave motion) `(motion ,(send event get-x)
                                           ,(send event get-y)))
            ((left-down)          '(button left   #t))
            ((left-up)            '(button left   #f))
            ((middle-down)        '(button middle #t))
            ((middle-up)          '(button middle #f))
            ((right-down)         '(button right  #t))
            ((right-up)           '(button right  #f))))
        (super on-event event))
      (define/override (on-char event)
        (define key-code (send event get-key-code))
        (define key-code-release (send event get-key-release-code))
        (on-key
          (case key-code
            ((release) `(up   ,key-code-release))
            (else      `(down ,key-code))))
        (super on-char event))))
  (lambda ()
    (define frame  (new widget-frame% (label "")))
    (define canvas (new widget-canvas% (parent frame)))
    (method-lambda
      ((set-title! title)    (send frame set-label title))
      ((show)                (send frame show #t))
      ((hide)                (send frame show #f))
      ((resize width height) (send frame resize width height))
      ((paint painter)       (send canvas refresh-now (lambda (dc) (painter dc)))))))

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
