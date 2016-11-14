
;Archivo creado en clases para generar funciones
(define ns (make-base-namespace))

(define operadores '(+ *))
(define operandos '(a b x y))

(define elemento
  (lambda (L)
    (list-ref L (random (length L)))))

(define expresion
  (lambda (n)
    (cond ((zero? n) (elemento operandos))
          (else
           (list
            (elemento operadores)
            (expresion (random n))
            (expresion (random n)))
           ))))

(define generar
  (lambda (n p)
    (if (zero? n) empty
        (cons
         (list 'λ '(x y) (expresion p))
         (generar (- n 1) p))
        )))
