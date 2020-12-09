#lang racket/base
(provide tcp:listener tcp:connect)
(require "method.rkt" racket/tcp)

(define (socket in out)
  (method-lambda
    ((close)        (close-input-port  in)
                    (close-output-port out))
    ((close-input)  (close-input-port  in))
    ((close-output) (close-output-port out))
    ((read)         (read in))
    ((write datum)  (write datum out) (flush-output out))))

(define (tcp:listener port (max-allow-wait 4))
  (define l (tcp-listen port max-allow-wait #t))
  (method-lambda
    ((close)  (tcp-close l))
    ((accept) (define-values (in out) (tcp-accept l))
              (socket in out))))

(define (tcp:connect host port)
  (define-values (in out) (tcp-connect host port))
  (socket in out))


;; example server steps

;(define l (tcp:listener 8123))
;(define s (l 'accept))
;(s 'write "welcome to the server")
;(s 'read)
;(s 'close)


;; example client steps

;(define s (tcp:connect  "104.175.140.40" 8123))
;(s 'read)
;(s 'write "why thank you, I am excited to be here")
;(s 'close)
