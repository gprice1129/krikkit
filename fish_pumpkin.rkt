#lang racket/base
(provide
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
  (define es (unbox new-events))
  (let loop ()
    (unless (box-cas! new-events es (cons (cons tag event) es))
      (loop))))
(define (pop-events!)
  (define es (unbox new-events))
  (let loop ()
    (unless (box-cas! new-events es '())
      (loop)))
  es)

(define wmaker (widget-maker (event-pusher 'key) (event-pusher 'mouse)))
(define window (wmaker))
(window 'set-title! "testing")
(window 'resize 400 300)
(window 'show)

(define pos-x 0)
(define pos-y 0)

(void (thread (lambda ()
        (let loop ()
          (for ((event (pop-events!)))
            (match event
              (`(mouse motion ,x ,y) (set! pos-x x) (set! pos-y y))
              (_ (pretty-print event))))
          (window 'paint (lambda (dc) (draw-pict joined-image dc pos-x pos-y)))
          (sleep 0)
          (loop)))))