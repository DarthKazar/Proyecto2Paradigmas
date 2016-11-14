

(define f
  (λ (x y)
    (+ (* (expt x 2) (- 1 y))
       (/ (* 4 (expt y x)) (+ 3 (* 2 y)))
       )
    )
  )

(define q
  (lambda (f L)
    (map 
     (lambda (p)
       (list p (f (car p) (cadr p))))
     L)
    ))

