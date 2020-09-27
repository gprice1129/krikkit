#lang racket/base
(provide window event-queue (all-from-out "method.rkt"))
(require racket/class racket/gui/base racket/match "method.rkt")

(define (window on-key on-mouse width height)
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
  (define frame  (new frame% (label "") (width width) (height height)))
  (define canvas (new widget-canvas% (parent frame)))
  (define bm  (make-object bitmap% (send frame get-width) (send frame get-height)))
  (define bdc (new bitmap-dc% (bitmap bm)))
  (method-lambda
    ((set-title! title)     (send frame set-label title))
    ((show)                 (send frame show #t))
    ((hide)                 (send frame show #f))
    ((resize width height)  
      
      ;; TODO: recreate back buffer
      (send frame resize width height)  
    )
    ((set-background r g b) (send bdc set-background (make-object color% r g b)))
    ((render draw)
      (send bdc clear)
      (draw bdc) 
      (send canvas refresh-now (lambda (dc) (send dc draw-bitmap bm 0 0))))))

;; thread-safe event queue
(define (event-queue)
  (define new-events (box '()))
  (method-lambda 
    ((push event) 
      (let loop ()
        (define es (unbox new-events))
        (unless (box-cas! new-events es (cons event es))
          (loop))))
    ((pop)
      (let loop ()
       (define es (unbox new-events))
         (if (box-cas! new-events es '())
           (reverse es)
           (loop))))))