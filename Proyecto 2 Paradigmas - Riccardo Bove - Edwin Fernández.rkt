#lang racket
(define ns (make-base-namespace))
#|
Proyecto 2 Paradigmas de Programación.
Generador de funciones por medio de algoritmo genético.
II Ciclo 2016.
Prof. Georges Alfaro.
Estudiantes:
-Riccardo Bove - 116310708.
-Edwin Fernández Fernández -304560080.
|#

;***********************DEFINICIONES*******************************
(define operadores '(+ * - / expt))
(define operandos '(x y a b))
;Operadores y operandos básicos utilizados por el generador.
(define parametros '(5 3 2 1 4))
;Parametros: (población,generaciones,desviación,elitismo,mutaciones).
;******************************************************************

;------------------------------------------------------------------

;**********************FUNCIONES BASE******************************
(define elemento (lambda (L) (list-ref L (random (length L))) ) )
;Función base para obtener un operador u operando al azar.

(define expresion (lambda (n) (cond ((zero? n) (elemento operandos)) (else
    (list (elemento operadores) (expresion (random n))(expresion (random n)) ) ) ) ) )
;Generador base de expresiones aritméticas (polinomios).

(define generador (lambda (n p)	(if (zero? n) empty
			(cons (list 'lambda '(x y a b) (expresion p)) (generador (- n 1) p)) )))
;Generador base de funciones aritméticas y potencia.

(define cruces (lambda (f1 f2) (list (car f1)(cadr f1)(caddr f2)) ) )
;Cruza dos expresiones, lado izq. de una con el der. de la otra.

(define toList (list (elemento operadores)) )
;Convierte un elemento al azar en una lista de un elemento.
;******************************************************************

;------------------------------------------------------------------

;********************FUNCIONES DERIVADAS***************************
(define mutar (lambda (expr)
    (append toList (cdr expr)) ))
;Mutación simple de una expresión que reemplaza el nodo raíz.

(define q
  (lambda (f L)
    (map 
     (lambda (p)
       (list p (f (car p) (cadr p))))
     L)
    ))
;Generador de valores de entrada a la funcion.

(define fitness
  (lambda (L1 L2)
    (cond
      [(or (null? L1) (null? L2)) '()]
      [(and (null? (cdr L1)) (null? (cdr L2))) (expt (- (car L2) (car L1)) 2)]
      [(+ (expt (- (car L2) (car L1)) 2) (fitness (cdr L1) (cdr L2)))]
        )
    )
  )
;Función de ajuste para funciones derivadas, determina viabilidad.

;******************************************************************

;------------------------------------------------------------------

;*************************PRUEBAS**********************************
;(elemento operadores)
;(elemento operando)
;(expresion 2)
;(expresion 3)
;(generador 2 2)
;(generador 2 3)
;(cruces (expresion 2) (expresion 3) )
;(define expr '(+ (* (expt x 2) (- 1 y)) 1))
;expr
;(mutar expr)

;**********Funcion de ejemplo**********
#|(define f
  (lambda (x y)
    (+ (* (expt x 2) (- 1 y))
       (/ (* 4 (expt y x)) (+ 3 (* 2 y)))
       )
    )
  )|#
;**************************************
;(generador 2 2)
;(q f '((1 1) (1 2) (2 2) (1 3)))
;((eval (car (generador 2 2)) ns) 1 1 (random) (random))
;(define l1 (list 3 4 7 6))
;(define l2 (list 3 3 6 6))
;(fitness l1 l2)
;******************************************************************
