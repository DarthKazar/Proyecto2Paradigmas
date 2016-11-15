#lang racket
#|
Proyecto 2 Paradigmas de Programación.
Generador de funciones por medio de algoritmo genético.
II Ciclo 2016.
Prof. Georges Alfaro.
Estudiantes:
-Riccardo Bove.
-Edwin Fernández Fernández -304560080.
|#

;***********************DEFINICIONES*******************************
(define operadores '(+ * - / expt))
(define operandos '(a b x y))
;Operadores y operandos básicos utilizados por el generador.
;******************************************************************

;------------------------------------------------------------------

;**********************FUNCIONES BASE******************************
(define elemento (lambda (L) (list-ref L (random (length L))) ) )
;Función base para obtener un operador u operando al azar.

(define expresion (lambda (n) (cond ((zero? n) (elemento operandos)) (else
    (list (elemento operadores) (expresion (random n))(expresion (random n)) ) ) ) ) )
;Generador base de expresiones aritméticas (polinomios).

(define generador (lambda (n p)	(if (zero? n) empty
			(cons (list 'λ '(x y) (expresion p)) (generador (- n 1) p)) )))
;Generador base de funciones aritméticas y potencia.
;******************************************************************

;------------------------------------------------------------------

;**********************PRUEBAS******************************
;(elemento operadores)
;(elemento operando)
;(expresion 2)
;(expresion 3)
;(generador 2 2)
;(generador 2 3)
;******************************************************************

