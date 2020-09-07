#lang racket/base
(provide widget-maker)
(require racket/class racket/gui/base racket/match "method.rkt")
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