#lang racket/base
(require racket/pretty "fish_pumpkin_state.rkt" "network.rkt")
(module+ main
  ;(define (handle-events st)
    ;(foldl
      ;(lambda (event st)
        ;(define as (event->actions event))
        ;(handle-actions st as))
      ;st
      ;(evq 'pop)))

  ;(define (replayer actions)
    ;(lambda (st)
      ;(if (null? actions)
          ;st
          ;(let ()
            ;(define a (car actions))
            ;(set! actions (cdr actions))
            ;(sleep 0.1)
            ;(st 'handle-action a)))))

  ;(define (stop? st) (st 'winning?))

  ;(void (thread (lambda ()
          ;(define st.init (sokoban-state map.w1))
          ;(define actions ((game-loop st.init handle-events world-present! stop?) 'history))
          ;(define replay-action! (replayer actions))
          ;(pretty-print `(you won! using these actions: ,actions))
          ;(game-loop st.init replay-action! world-present! stop?)
          ;(pretty-print '(replay over hope you liked it!))))))
    (define listener (tcp:listener 8124))
    ((((let loop ((connections '()))
        (for-each (lambda (conn)
            (conn 'write "New player joined")) connections)
        (loop (append (list (listener 'accept)) connections)))))))
        