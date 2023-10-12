;; Proyecto: Calculadora Lisp
;; Autor; Cesar Gonzalez Lopez
;; Fecha: 11/10/2023


;; entorno grafico
(defun Calculadora ()
    (print "Calculadora")
    (print "Bienvenido a la calculadora de cesar para el proyecto de paradigmas ")
    (print "primer parcial")
    (print "ingrese el la operacion numero")
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
                    (t (error "Operación no válida"))
                )
                resultado
            )
        )
        (t (error "Expresión no válida"))
    )
)
