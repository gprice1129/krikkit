    (define player-x (remainder player-index width))
      (define player-y (quotient player-index width))

      (define (can-move-from x y dir) 
        (define (shift base neg-dir pos-dir)
            (+ base (case direction (neg-dir -1) (pos-dir 1) (else 0))))
        (define target-x (shift player-x 'left 'right))
        (define target-y (shift player-y 'down 'up))
        (define target-index (+ (* target-y width) target-x))
        (define target-tile (vector-ref grid target-index))
        (if (member 'wall target-tile) #f
          
          )

      ;; translate direction to target coordinate
      (define (shift base neg-dir pos-dir)
        (+ base (case direction (neg-dir -1) (pos-dir 1) (else 0))))
      (define target-x (shift player-x 'left 'right))
      (define target-y (shift player-y 'down 'up))
      ;#(define target-y (shift player-y (or (eq? direction 'up') (eq? direction 'down))))

      (define target-index (+ (* target-y width) target-x))
      (define player-tile (vector-ref grid player-index))
      (define target-tile (vector-ref grid target-index))
      (cond ((member 'wall tile) #f)
            ((and (member 'pumpkin player-tile) (member 'pumpkin target-index)) #f)
            
          (begin #t #|TODO move player|#)