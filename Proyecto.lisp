; Proyecto: Calculadora Lisp
; Autor; Cesar Gonzalez Lopez
; Fecha: 11/10/2023


; entorno grafico
(defun Calculadora ()
    (print "Calculadora")
    (print "Bienvenido a la calculadora de cesar para el proyecto de paradigmas ")
    (print "primer parcial")
    (print "ingrese la operacion")
    (print "")
    (setq operacion (read))

    (evaluacion operacion)
)
;funcion de evaluacion
(defun evaluacion (expresion)
    (cond
        ((numberp expresion) expresion)
        ((listp expresion)
            (let* 
                (
                    ;comparacion entre valores, toma como referencia una lista de
                    ;3 elementos, el primer valor, la operacion y el segundo valor
                    ;si el tercer valor es nulo, se toma como 0
                    (primerValor (evaluacion (first expresion)))
                    (operacion (second expresion))
                    (segundoValor (if (third expresion) (evaluacion (third expresion)) 0)) 
                    resultado
                )
                
                (case operacion
                    ((+) (setq resultado (+ primerValor SegundoValor)))
                    ((-) (setq resultado (- primerValor SegundoValor)))
                    ((*) (setq resultado (* primerValor SegundoValor)))
                    ((/) (setq resultado (/ primerValor SegundoValor)))
                    ((r) (setq resultado (sqrt primerValor)))
                    ((^) (setq resultado (expt primerValor segundoValor)))
                    (t (error "Operacion no valida"))
                )
                resultado
            )
        )
        (t (error "Expresion no valida"))
    )
)





;((40 / 10) + ((5 - 2) ^ 3))
;((3 + 5) * ((2 - 1) * 3))
