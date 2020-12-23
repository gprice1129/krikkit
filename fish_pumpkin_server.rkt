#lang racket/base
(require racket/pretty "fish_pumpkin_state.rkt" "network.rkt")
(module+ main
    ; TODO
    ; player specific actions
    ; assume no erros (for now)
    ; send/get actions
    ; check for new connections
    ; remove quitters
    (define listener (tcp:listener 8124))
    (let loop ((connections '()))
        (for-each (lambda (conn)
            (conn 'write "New player joined")) connections)
        (loop (append (list (listener 'accept)) connections))))
        