#lang racket

;;Función miembro
;;Retorna #t o #f si el elemento ingresado se encuentra en la lista
(define (miembro? elemento lista)
  (cond ((null? lista)#f)
   ((equal? (car lista) elemento) #t)
  (else (miembro? elemento (cdr lista)))))

;;Retorna la fila indicada, necesita el numero de fila y la matriz
;;Ademas returna una lista vacia si el la fila es mayor a las que tiene la matriz
(define (filaMat fila matriz)
    (cond ((null? matriz) 
        '())
    ((= fila 1) 
            (car matriz))
    (else 
        (filaMat (- fila 1) (cdr matriz)))))
;;(filaMat 2 '((1 2) (3 4) (5 6) (7 8)))

;;Retorna #t o #f si el elemeento recibido es una matriz o no
(define (matriz? matriz)
    (cond ((null? matriz)
    #f)
    ((list? (car matriz))
        #t)   
    (else
        #f)
    ))

;;Función para determinar el largo de una matriz o una lista
(define (len matriz)
    (cond((null? matriz)
        0
    )
    (else 
    (+ (len (cdr matriz)) 1))))

;;Dependiendo de la fila devuelve el elemento necesario para formar la diagonal de la matriz
(define (auxdiag matriz fila)
    (cond((null? matriz)
    '())
    ((matriz? matriz)
        (cond((= fila 1) 
            (caar matriz))
            (else
            (auxdiag (filaMat fila matriz) fila))
        )
    )
    ((= fila 1)
    (car matriz))
    (else
    (auxdiag (cdr matriz) (- fila 1)))
    )
)

;(auxdiag '((1 2 3) (3 4 5) (5 6 7) (7 8 9)) 1)

;;Función diag retorna una lista con la primera diagonal de la matriz
(define (diag matriz cont) 
    (cond ((or (= cont (+ (len matriz) 1)) (= cont (+ (len (car matriz)) 1)))
        '())
    (else
        (append (list (auxdiag matriz cont))(diag matriz (+ cont 1)))
    )))


;;Función auxiliar de matdiagR y falsaTraspuesta y elimina el primer elemento de cada fila de la matriz
(define (elIniMat matriz)
    (cond ((null? matriz)
    '())
    (else
    (append (list (cdar matriz)) (elIniMat (cdr matriz)))))
)

;;Función auxiliar falsaTraspuesta, saca el primer elemento de todas las filas
(define (sacIniMat matriz)
    (cond ((null? matriz)
    '())
    (else
    (append (list (caar matriz)) (sacIniMat (cdr matriz)))))
)

;;Retorna una matriz de las diagonales de la parte izquierda de la matriz principal
(define (matdiagL matriz) 
    (cond((null? matriz)
    '())
    (else
    (append (list (diag matriz 1)) (matdiagL (cdr matriz)))))
)

;;Retorna una matriz de las diagonales de la parte derecha de la matriz principal
(define (matdiagR matriz) 
    (cond((or (null? matriz) (null? (car matriz)))
    '())
    (else
    (append (list (diag matriz 1)) (matdiagR (elIniMat matriz)))))
)
;;Retorna una matriz con las listas de todas las diagonales de la matriz
(define (matdiag matriz)
    (append (matdiagL matriz)(cdr (matdiagR matriz)))
)
;;Retorna la falsaTraspuesta de una matriz
;;Se llama falsa traspuesta debido a que invierte el orden de la misma para poder analizar las diagonales de la matriz
(define (falsaTraspuesta matriz)
    (cond ((null? (car matriz))
    '())
    (else
        (cons (sacIniMat (reverse matriz)) (falsaTraspuesta (elIniMat matriz)))))
)
;;retorna una lista con dos matrices para ingresarla al greedy, una de ellas es la matriz principal y la otra son las diagonales
(define (candidatos matriz)
    (append (list matriz)  (append (list(matdiag matriz)) (list(matdiag (falsaTraspuesta matriz)))))
)

;;Función auxiliar de seleccion, busca posibles lineas donde colocar fichas, estu funcion busca pares de fichas fichas en 101
(define (linea2? lista)
    (cond ((null? (cdr lista))
        #f
    )
    ((null? (cddr lista))
        #f
    )    
    ((or (= (car lista) 1) (= (car lista) 2))
        (cond
        ((or (= (car lista) (cadr lista)) (and (= (car lista) (caddr lista)) (= (cadr lista) 0)))
            #t
        )(else
        (linea2? (cdr lista))
        )))
    ((or (= (car (reverse lista)) 1) (= (car (reverse lista)) 2))
        (cond
        ((or (= (car (reverse lista)) (cadr (reverse lista))) (and (= (car (reverse lista)) (caddr (reverse lista))) (= (cadr (reverse lista)) 0)))
            #t
        )(else
        (linea2? (cdr (reverse lista)))
        )))
    (else
        (linea2? (cdr lista))
        )
))

;;Selecciona posibles soluciones
(define (seleccion candidatos)
    (append (list(filter linea2? (car candidatos))) (list(filter linea2? (cadr candidatos))) (list(filter linea2? (caddr candidatos))))
)
;;De las posibles soluciones escoge cuales son viables para ser una solucion
(define (viabilidadAux? lista)
    (cond 
    ((null?(cdddr lista))
    #f
    )
    ((or (= (car lista) 1) (= (car lista) 2))
            (cond((and (=(car lista) (caddr lista)) (= (cadr lista) 0) (=(cadddr lista) 0))
                #t)
            ((and (=(car lista) (cadr lista)) (= (caddr lista) 0) (=(cadddr lista) 0))
                #t)
            ((and (=(car lista) (cadr lista) (caddr lista)) (=(cadddr lista) 0))
                #t)
            (else
        (viabilidadAux? (cdr lista))))
        )
    ((or (= (car (reverse lista)) 1) (= (car (reverse lista)) 2))
        (cond((and (=(car (reverse lista)) (caddr (reverse lista))) (= (cadr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
                #t)
            ((and (=(car (reverse lista)) (cadr (reverse lista))) (= (caddr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
                #t)
            ((and (=(car (reverse lista)) (cadr (reverse lista)) (caddr (reverse lista))) (=(cadddr (reverse lista)) 0))
                #t)
            (else
        (viabilidadAux? (cdr (reverse lista)))))
        )

    (else
        (cond
        ((null? (cddddr lista))
            (viabilidadAux? (cdr lista))
        )
        ((and (=(car lista) 0) (and (or (=(cadr lista) 1) (=(cadr lista) 2)) (and (=(cadr lista) (cadddr lista)) (= (caddr lista) 0))))
            #t
        )
        ((and (=(car lista) 0) (and (or (=(cadr lista) 1) (=(cadr lista) 2)) (and (=(cadr lista) (caddr lista)) (= (cadddr lista) 0))))
            #t
        )
        ((and (=(car lista) 0) (and (or (=(cadr lista) 1) (=(cadr lista) 2)) (and (=(cadr lista) (caddr lista) (cadddr lista)))))
            #t
        )
        (else (viabilidadAux? (cdr lista)))
        ))
))
;;De las posibles soluciones escoge cuales son viables para ser una solucion
(define (viabilidad candidatos)
    (cond ((null? candidatos)
        '())
        (else
        (append (list (filter viabilidadAux?  (car candidatos))) (viabilidad (cdr candidatos)))
    ))
)


;;Asigna un valor numerico a las posibles soluciones
(define (objetivoAux lista)
    (cond 
    ((= (car lista) 1) 
            (cond((and (=(car lista) (caddr lista)) (= (cadr lista) 0) (=(cadddr lista) 0))
                (list 2))
            ((and (=(car lista) (cadr lista)) (= (caddr lista) 0) (=(cadddr lista) 0))
                (list 1))
            ((and (=(car lista) (cadr lista) (caddr lista)) (=(cadddr lista) 0))
                (list 3))
            (else
        (objetivoAux (cdr lista))))
        )
    ((= (car lista) 2)
            (cond((and (=(car lista) (caddr lista)) (= (cadr lista) 0) (=(cadddr lista) 0))
                (list -2))
            ((and (=(car lista) (cadr lista)) (= (caddr lista) 0) (=(cadddr lista) 0))
                (list -1))
            ((and (=(car lista) (cadr lista) (caddr lista)) (=(cadddr lista) 0))
                (list -3))
            (else
        (objetivoAux (cdr lista))))
       )
    ((= (car (reverse lista)) 1)
        (cond((and (=(car (reverse lista)) (caddr (reverse lista))) (= (cadr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
                (list 2))
            ((and (=(car (reverse lista)) (cadr (reverse lista))) (= (caddr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
                (list 1))
            ((and (=(car (reverse lista)) (cadr (reverse lista)) (caddr (reverse lista))) (=(cadddr (reverse lista)) 0))
                (list 3))
            (else
        (objetivoAux (cdr (reverse lista)))))
        )
    ((= (car (reverse lista)) 2)
        (cond((and (=(car (reverse lista)) (caddr (reverse lista))) (= (cadr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
                (list -2))
            ((and (=(car (reverse lista)) (cadr (reverse lista))) (= (caddr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
                (list -1))
            ((and (=(car (reverse lista)) (cadr (reverse lista)) (caddr (reverse lista))) (=(cadddr (reverse lista)) 0))
                (list -3))
            (else
        (objetivoAux (cdr (reverse lista)))))
        )

    (else
        (objetivoAux (cdr lista))))

)

;;Concatena el valor numerico en una lista para despues ser evaluada
(define (aplicarObj lista)
    (cond ((null? lista)
    '())
    (else
        (append (list(append (list(car lista)) (objetivoAux (car lista)))) (aplicarObj (cdr lista)))
    )
    )
)
;;Asigna un valor numerico a las posibles soluciones
(define (objetivo lista)
    (cond ((null? lista)
        '()
    )(else
        (append (aplicarObj (car lista)) (objetivo (cdr lista)))
    )
    )
)



(viabilidad(seleccion (candidatos '(
    (0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0)
    (0 1 1 0 0 0 0 0)
    (0 0 0 0 1 0 0 0)
    (0 1 1 1 2 0 0 0)))))



