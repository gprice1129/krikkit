#lang racket/base
(provide

)
(require pict pict/flash racket/draw racket/class racket/gui/base racket/pretty)

(define-syntax pict+code
  (syntax-rules ()
    ((_ e) (hc-append 10 e (code e)))))

(define pos-x 0)
(define pos-y 0)

(define image (standard-fish 100 45 #:open-mouth #t #:color (make-color 128 0 128)))
(define image2 (jack-o-lantern 100 (make-color 128 0 128)))
(define joined-image (hc-append image2 image))

(define game-frame%
  (class frame%
    (init) (super-new)
    (define/override (on-subwindow-event receiver event)
      (define m-y (send event get-y))
      (define m-x (send event get-x))
      (pretty-print `(,(send event get-event-type)
                      ,m-x ,m-y))
      (set! pos-x m-x)
      (set! pos-y m-y)
      (send game-canvas refresh)
      (super on-subwindow-event receiver event))
    (define/override (on-subwindow-char receiver event)
      (define key-code (send event get-key-code))
      (define key-release-code (send event get-key-release-code))
      (pretty-print `(,key-code ,key-release-code))
      (super on-subwindow-char receiver event))))

;(define image (colorize (filled-flash 50 30) "yellow"))
(define (on-paint canvas dc)
  (pretty-print `(we are here ,pos-x ,pos-y))
  (draw-pict joined-image dc pos-x pos-y)
  (send dc draw-text "Hello, World!" 5 1))

(define frame
  (new game-frame% 
    (label "stuff") 
    (width 300) 
    (height 300) 
    (alignment '(center center))))

(define game-canvas
  (new canvas% 
    (parent frame)
    (paint-callback on-paint)))

#;(define (add-drawing p)
  (let ((drawer (make-pict-drawer p)))
    (new canvas% 
      (parent frame) 
      (style '(border)) 
      (paint-callback (lambda (self dc) (drawer dc 0 0)))
      )))

(send frame show #t)