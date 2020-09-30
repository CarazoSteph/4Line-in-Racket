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

(define (vertical matriz)
    (cond ((null? (car matriz))
        '()
    )(else
        (append (list (sacIniMat matriz)) (vertical (elIniMat matriz)))
    )  
    )
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
    (append (list matriz)  (append (list(matdiag matriz)) (list(matdiag (falsaTraspuesta matriz))) (list (vertical matriz))))
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
    (append (list(filter linea2? (car candidatos))) (list(filter linea2? (cadr candidatos))) (list(filter linea2? (caddr candidatos))) (list(filter linea2? (cadddr candidatos))))
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
    ((null? (cdr lista)) '())
    ((null? (cddr lista)) '())
    ((null? (cdddr lista)) '())
    ((= (car lista) 1) 
            (cond
            ((and (=(car lista) (caddr lista)) (=(cadr lista) 0) (= (cadddr lista) 0))
                (append (list -2) (objetivoAux (cddr lista))))
            ((and (=(car lista) (cadr lista)) (=(caddr lista) 0) (= (cadddr lista) 0))
                (append (list -1) (objetivoAux (cddr lista))))
            ((and (=(car lista) (cadr lista) (caddr lista)) (= (cadddr lista) 0))
                (append (list -3) (objetivoAux (cddr lista))))
            ((and (=(car lista) (cadddr lista) (caddr lista)) (= (cadr lista) 0))
                (append (list -3) (objetivoAux (cddr lista))))
            (else
        (objetivoAux (cdr lista))))
        )
    ((= (car lista) 2)
            (cond
            ((and (=(car lista) (caddr lista)) (=(cadr lista) 0) (= (cadddr lista) 0))
                (append (list 2) (objetivoAux (cddr lista))))
            ((and (=(car lista) (cadr lista)) (=(caddr lista) 0) (= (cadddr lista) 0))
                (append (list 1) (objetivoAux (cddr lista))))
            ((and (=(car lista) (cadr lista) (caddr lista)) (= (cadddr lista) 0))
                (append (list 3) (objetivoAux (cddr lista))))
            ((and (=(car lista) (cadddr lista) (caddr lista)) (= (cadr lista) 0))
                (append (list 3) (objetivoAux (cddr lista))))
            (else
        (objetivoAux (cdr lista))))
       )
    ((= (car (reverse lista)) 1)
        (cond
            ((and (=(car (reverse lista)) (caddr (reverse lista))) (=(cadr (reverse lista)) 0) (= (cadddr (reverse lista)) 0))
                (append (list -2) (objetivoAux (cddr (reverse lista)))))
            ((and (=(car (reverse lista)) (cadr (reverse lista))) (=(caddr (reverse lista)) 0) (= (cadddr (reverse lista)) 0))
                (append (list -1) (objetivoAux (cddr (reverse lista)))))
            ((and (=(car (reverse lista)) (cadr (reverse lista)) (caddr (reverse lista))) (= (cadddr (reverse lista)) 0))
                (append (list -3) (objetivoAux (cddr (reverse lista)))))
            ((and (=(car (reverse lista)) (cadddr (reverse lista)) (caddr (reverse lista))) (= (cadr (reverse lista)) 0))
                (append (list -3) (objetivoAux (cddr (reverse lista)))))
            (else
        (objetivoAux (cdr (reverse lista)))))
        )
    ((= (car (reverse lista)) 2)
        (cond
            ((and (=(car (reverse lista)) (caddr (reverse lista))) (=(cadr (reverse lista)) 0) (= (cadddr (reverse lista)) 0))
                (append (list 2) (objetivoAux (cddr (reverse lista)))))
            ((and (=(car (reverse lista)) (cadr (reverse lista))) (=(caddr (reverse lista)) 0) (= (cadddr (reverse lista)) 0))
                (append (list 1) (objetivoAux (cddr (reverse lista)))))
            ((and (=(car (reverse lista)) (cadr (reverse lista)) (caddr (reverse lista))) (= (cadddr (reverse lista)) 0))
                (append (list 3) (objetivoAux (cddr (reverse lista)))))
            ((and (=(car (reverse lista)) (cadddr (reverse lista)) (caddr (reverse lista))) (= (cadr (reverse lista)) 0))
                (append (list 3) (objetivoAux (cddr (reverse lista)))))
            (else
        (objetivoAux (cdr (reverse lista))))))
    (else
    (cond
        ((null? (cddddr lista))
            (objetivoAux (cdr lista))
        )
        ((and (=(car lista) 0) (and (=(cadr lista) 1) (and (=(cadr lista) (cadddr lista)) (= (caddr lista) 0))))
            (append (list -2) (objetivoAux (cdddr lista))))
        ((and (=(car lista) 0) (and (=(cadr lista) 1) (and (=(cadr lista) (caddr lista)) (= (cadddr lista) 0))))
            (append (list -1) (objetivoAux (cdddr lista))))
        ((and (=(car lista) 0) (and (=(cadr lista) 1) (and (=(cadr lista) (caddr lista) (cadddr lista)))))
            (append (list -3) (objetivoAux (cdddr lista))))
        ((and (=(car lista) 0) (and (=(cadr lista) 2) (and (=(cadr lista) (cadddr lista)) (= (caddr lista) 0))))
            (append (list 2) (objetivoAux (cdddr lista))))
        ((and (=(car lista) 0) (and (=(cadr lista) 2) (and (=(cadr lista) (caddr lista)) (= (cadddr lista) 0))))
            (append (list 1) (objetivoAux (cdddr lista))))
        ((and (=(car lista) 0) (and (=(cadr lista) 2) (and (=(cadr lista) (caddr lista) (cadddr lista)))))
            (append (list 3) (objetivoAux (cdddr lista))))
        (else 
        (objetivoAux (cdr lista))))))

)

;;Concatena el valor numerico en una lista para despues ser evaluada
(define (aplicarObj lista)
    (cond ((null?  lista)
    '())
    (else
        (append (list(append (list(car lista)) (list(objetivoAux (car lista))))) (aplicarObj (cdr lista)))
    )
    )
)
;;Asigna un valor numerico a las posibles soluciones
(define (objetivo lista)
    (cond ((null? lista)
        '())
    (else
        (append (list(aplicarObj (car lista))) (objetivo (cdr lista)))
    )
    )
)
;;Ejecuta la funcion selec la cual agrupa las de indice 3
(define (buscSelec3 matriz)
    (cond ((null? matriz)
    '()
    )(else
        (append (list(selec3 (car matriz))) (buscSelec3 (cdr matriz)))
)))
;;Ejecuta la funcion selec la cual agrupa las de indice 2
(define (buscSelec2 matriz)
    (cond ((null? matriz)
    '()
    )(else
        (append (list(selec2 (car matriz))) (buscSelec2 (cdr matriz)))
)))
;;Ejecuta la funcion selec la cual agrupa las de indice 1
(define (buscSelec1 matriz)
    (cond ((null? matriz)
    '()
    )(else
        (append (list(selec1 (car matriz))) (buscSelec1 (cdr matriz)))
)))
;;Busca en las soluciones objetivo las que tienen indice #3
(define (selec3 lista)
    (cond 
    ((null? lista)
    '()
    )
    ((miembro? 3 (cadar lista))
        (append (list(caar lista)) (selec3 (cdr lista)))
    )
    ((miembro? -3 (cadar lista))
        (append (list(list(caar lista))) (selec3 (cdr lista)))
    )
    (else
        (selec3 (cdr lista))
    ))
)
;;Busca en las soluciones objetivo las que tienen indice #2
(define (selec2 lista)
    (cond
    ((null? lista)
    '()
    )
    ((miembro? 2 (cadar lista))
        (append (list(caar lista)) (selec2 (cdr lista)))
    )
    ((miembro? -2 (cadar lista))
        (append (list(list(caar lista))) (selec2 (cdr lista)))
    )       
    (else
        (selec2 (cdr lista))
    )))
;;Busca en las soluciones objetivo las que tienen indice #1
(define (selec1 lista)
    (cond
    ((null? lista)
        '()
    )
    ((miembro? 1 (cadar lista))
            (append (list(caar lista)) (selec1 (cdr lista)))
    )
    ((miembro? -1 (cadar lista))
            (append (list (list(caar lista))) (selec1 (cdr lista)))
    )
    (else
        (selec1 (cdr lista))
    )))
;;Funcion selec sirve para buscar las soluciones con heuristico especifico
(define (selec matriz cont)
    (cond 
    ((= cont 3)
        (buscSelec3 matriz)
    )
    ((= cont 2)
        (buscSelec2 matriz)
    )
    ((= cont 1)
        (buscSelec1 matriz)
    ))
)
;;Función jugadas ejecuta todas las funciones del algoritmo voraz para agrupar las soluciones mas viables
(define (jugadas matriz cont)
    (selec (objetivo (viabilidad (seleccion (candidatos matriz)))) cont)
)
;;Funcion auxiliar util para encontrar la columna donde se desea insertar la ficha
(define (columnaSol lista)
    (cond ((null? (cdr lista))
    0)
    ((or (=(car lista) 1) (=(car lista) 2))
        (cond 
        ((and (= (car lista) (cadr lista) (caddr lista)) (=(cadddr lista) 0))
            3)
        ((and (= (car lista) (cadr lista)) (=(caddr lista) 0) (=(cadddr lista) 0))
            2)
        ((and (= (car lista) (caddr lista)) (=(cadr lista) 0) (=(cadddr lista) 0))
            1)
        (else
            (+ (columnaSol (cdr lista)) 1)
        ))
    )
    ((and (= (car lista) 0) (not(zero?(cadr lista))))
        (cond 
        ((and (= (cadr lista) (caddr lista) (cadddr lista)))
            0)
        ((and (= (cadr lista) (caddr lista)) (=(cadddr lista) 0))
            3)
        ((and (= (cadr lista) (cadddr lista)) (=(caddr lista) 0))
            2)
        (else
            (+ (columnaSol (cdr lista)) 1)
        ))
    )
    ((or (=(car (reverse lista)) 1) (=(car (reverse lista)) 2))
        (cond 
        ((and (= (car (reverse lista)) (cadr (reverse lista)) (caddr (reverse lista))) (=(cadddr (reverse lista)) 0))
            (-(length lista) 4))
        ((and (= (car (reverse lista)) (cadr (reverse lista))) (=(caddr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
            (-(length lista) 3))
        ((and (= (car (reverse lista)) (caddr (reverse lista))) (=(cadr (reverse lista)) 0) (=(cadddr (reverse lista)) 0))
            (-(length lista) 2))
        (else
            (+ (columnaSol (cdr (reverse lista))) -1)
        ))
    )
    ((and (=(car (reverse lista)) 0) (not(zero?(cadr lista))))
        (cond 
        ((and (= (cadr (reverse lista)) (caddr (reverse lista)) (cadddr (reverse lista))))
            (-(length lista) 1))
        ((and (= (cadr (reverse lista)) (caddr (reverse lista))) (=(cadddr (reverse lista)) 0))
            (-(length lista) 4))
        ((and (= (cadr (reverse lista)) (cadddr (reverse lista))) (=(caddr (reverse lista)) 0))
            (-(length lista) 3))
        (else
            (+ (columnaSol (cdr (reverse lista))) -1)
        ))
    )
    (else
            (+ (columnaSol (cdr lista)) 1)
    )
    )
)
;;Valida las soluciones de la sublista de soluciones horizontales
(define (verHoriz matriz lista)
    (cond 
    ((null? lista)
    -1)
    ((not (list? (caar lista)))
        (cond 
        ((= (+(index-of matriz (car lista))1) (length (car matriz)))
            (columnaSol (car lista))
        )
        ((not (zero? (list-ref (list-ref matriz (+ (index-of matriz (car lista)) 1)) (columnaSol (car lista)))))
            (columnaSol (car lista)))
        (else 
        (verHoriz matriz (cdr lista)))
        )
        )
    (else 
        (cond
        ((= (+(index-of matriz (caar lista))1) (length (car matriz)))
            (columnaSol (caar lista))
        )
        ((not (zero? (list-ref (list-ref matriz (+ (index-of matriz (caar lista)) 1)) (columnaSol (caar lista)))))
            (columnaSol (caar lista)))
        (else 
        (verHoriz matriz (cdr lista)))
        )))
)
;;Valida las soluciones de la sublista de soluciones de diagonales de derecha a izquierda
(define (verDID matrizD lista)
    (cond 
    ((null? lista)
    -1)
    ((not (list? (caar lista)))
        (cond
            ((eq?  (length (car lista)) (round (/ (+(length matrizD) 1) 2)))
                (cond
                    ((or (< (columnaSol (car lista)) 1) (>= (columnaSol (car lista)) (length(list-ref matrizD (+ (index-of matrizD (car lista)) 1)))))
                        (columnaSol (car lista))
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (car lista)) 1)) (columnaSol (car lista)))))
                        (columnaSol (car lista)))
                    (else
                    (verDID matrizD (cdr lista))
                ))
            )
            ((> (index-of matrizD (car lista)) (round (/ (length matrizD) 2)))
                (cond
                    ((or (< (+(columnaSol (car lista))1) (>= (+(columnaSol (car lista))1) (length(list-ref matrizD (- (index-of matrizD (car lista)) 1)))))
                        (+(columnaSol (car lista)) (-  (+(round (/ (length matrizD) 2)) 1) (+(length (car lista))1)))
                    ))
                    ((not (zero?(list-ref (list-ref matrizD (- (index-of matrizD (car lista)) 1)) (+(columnaSol (car lista)) 1 ))))
                        (+(columnaSol (car lista)) (-  (+(round (/ (length matrizD) 2)) 1) (+(length (car lista))1))))
                    (else
                (verDID matrizD (cdr lista))
            ))
            )
            ((< (index-of matrizD (car lista)) (round (/ (length matrizD) 2)))
                (cond
                    ((or (< (columnaSol (car lista)) 1) (>= (columnaSol (car lista)) (length (list-ref matrizD (+ (index-of matrizD (car lista)) 1)))))
                        (columnaSol (car lista))
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (car lista)) 1)) (columnaSol (car lista)))))
                        (columnaSol (car lista)))
                    (else
                (verDID matrizD (cdr lista))
            ))
            )
                
            (else
                (verDID matrizD (cdr lista))
            )))
        (else
            (cond
                ((eq?  (length (caar lista)) (round (/ (+(length matrizD) 1) 2)))
                    (cond
                    ((or (< (columnaSol (caar lista)) 1)  (>= (columnaSol (caar lista)) (length(list-ref matrizD (+ (index-of matrizD (caar lista)) 1)))))
                        (columnaSol (caar lista))
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (caar lista)) 1)) (columnaSol (caar lista)))))
                        (columnaSol (caar lista)))
                    (else
                (verDID matrizD (cdr lista)))
                )
                )
                ((> (index-of matrizD (caar lista)) (round (/ (length matrizD) 2)))
                    (cond
                        ((or (< (+(columnaSol (caar lista))1) 1) (>= (+(columnaSol (caar lista))1) (length(list-ref matrizD (- (index-of matrizD (caar lista)) 1)))))
                        (+(columnaSol (caar lista)) (-  (+(round (/ (length matrizD) 2)) 1) (+(length (caar lista))1)))
                    )
                            ((not (zero?(list-ref (list-ref matrizD (- (index-of matrizD (caar lista)) 1)) (+(columnaSol (caar lista)) 1 ))))
                                (+(columnaSol (caar lista)) (- (length (caar lista)) (+(round (/ (length matrizD) 2)) 1) )))
                            (else
                        (verDID matrizD (cdr lista))
                    ))
                )
                ((< (index-of matrizD (caar lista)) (round (/ (length matrizD) 2)))
                    (cond
                    ((or (< (columnaSol (caar lista)) 1) (>= (columnaSol (caar lista)) (length (list-ref matrizD (+ (index-of matrizD (caar lista)) 1)))))
                        (columnaSol (caar lista))
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (caar lista)) 1)) (columnaSol (caar lista)))))
                        (columnaSol (caar lista)))
                    (else
                (verDID matrizD (cdr lista)))
                ))
                (else
                    (verDID matrizD (cdr lista))
                ))
                
                ))
)
;;Valida las soluciones de la sublista de soluciones de diagonales de izquierda a derecha
(define (verDDI matrizD lista)
    (cond 
    ((null? lista)
    -1)
    ((not (list? (caar lista)))
        (cond
            ((eq?  (length (car lista)) (round (/ (+(length matrizD) 1) 2)))
                (cond
                    ((or (< (-(columnaSol (car lista))1) 1) (>= (-(columnaSol (car lista))1) (length (list-ref matrizD (+ (index-of matrizD (car lista)) 1)))))
                        (columnaSol (car lista))
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (car lista)) 1)) (-(columnaSol (car lista))1))))
                        (columnaSol (car lista)))
                    (else
                (verDDI matrizD (cdr lista))
            ))
            )
            ((> (index-of matrizD (car lista)) (round (/ (length matrizD) 2)))
                (cond
                    ((or (< (columnaSol (car lista)) 1) (>= (columnaSol (car lista)) (length(list-ref matrizD (- (index-of matrizD (car lista)) 1)))))
                        (columnaSol (car lista))
                    )
                    ((not (zero?(list-ref (list-ref matrizD (- (index-of matrizD (car lista)) 1)) (columnaSol (car lista)) )))
                        (columnaSol (car lista)))
                    (else
                (verDDI matrizD (cdr lista))
            ))
            )
            ((< (index-of matrizD (car lista)) (round (/ (length matrizD) 2)))
                (cond
                    ((or (< (-(columnaSol (car lista))1) 1) (>= (-(columnaSol (car lista))1) (length(list-ref matrizD (+ (index-of matrizD (car lista)) 1)))))
                        (+(+(columnaSol (car lista)) (-  (+(round (/ (length matrizD) 2)) 1) (+(length (car lista))1)))1)
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (car lista)) 1)) (-(columnaSol (car lista))1))))
                        (+(+(columnaSol (car lista)) (-  (+(round (/ (length matrizD) 2)) 1) (+(length (car lista))1)))1))
                    (else
                (verDDI matrizD (cdr lista))
            ))
            )
                
            (else
                (verDDI matrizD (cdr lista))
            )))
        (else
            (cond
                ((eq?   (length (caar lista)) (round (/ (+(length matrizD) 1) 2)))
                    (cond
                    ((or (< (-(columnaSol (caar lista))1) 1) (>= (-(columnaSol (caar lista))1) (length (list-ref matrizD (+ (index-of matrizD (caar lista)) 1)))))
                        (columnaSol (caar lista))
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (caar lista)) 1)) (-(columnaSol (caar lista))1))))
                        (columnaSol (caar lista)))
                    (else
                (verDDI matrizD (cdr lista)))
                )
                )
                ((> (index-of matrizD (caar lista)) (round (/ (length matrizD) 2)))
                    (cond
                    ((or (< (columnaSol (caar lista)) 1) (>= (columnaSol (caar lista)) (length(list-ref matrizD (- (index-of matrizD (caar lista)) 1)))))
                        (columnaSol (caar lista))
                    )
                            ((not (zero?(list-ref (list-ref matrizD (- (index-of matrizD (caar lista)) 1)) (columnaSol (caar lista)) )))
                                (columnaSol (caar lista)))
                            (else
                        (verDDI matrizD (cdr lista))
                    ))
                )
                ((< (index-of matrizD (caar lista)) (round (/ (length matrizD) 2)))
                    (cond
                    ((or (< (-(columnaSol (caar lista))1) 1) (>= (-(columnaSol (caar lista))1) (length(list-ref matrizD (+ (index-of matrizD (caar lista)) 1)))))
                        (+(+(columnaSol (caar lista)) (-  (+(round (/ (length matrizD) 2)) 1) (+(length (caar lista))1)))1)
                    )
                    ((not (zero?(list-ref (list-ref matrizD (+ (index-of matrizD (caar lista)) 1)) (-(columnaSol (caar lista))1))))
                        (+(columnaSol (caar lista)) (- (length (caar lista)) (+(round (/ (length matrizD) 2)) 1) )))
                    (else
                (verDDI matrizD (cdr lista)))
                ))
                (else
                    (verDDI matrizD (cdr lista))
                ))
                ))
)
;;Funcion para verificar las soluciones verticales de la sublista de la sublista 
(define (verVert matrizV lista)
    (cond 
    ((null? lista)
    -1)
    ((not (list? (caar lista)))
        (index-of matrizV (car lista)))
    (else 
        (index-of matrizV (caar lista))
        ))
)
;;Funcion auxiliar de solucion busca y veerifica las soluciones para retornar la columna
(define (verSoluc matriz cont)
    (cond 
        ((= cont 0)
        (random (length (car matriz))))
        ((and (not (null? (car (jugadas matriz cont)))) (not(= (verHoriz matriz (car (jugadas matriz cont))) -1)))
            (verHoriz matriz (car (jugadas matriz cont)))
        )
        ((and(not (null? (cadr (jugadas matriz cont)))) (not(= (verDID  (matdiag matriz) (cadr (jugadas matriz cont))) -1)))
            (verDID (matdiag matriz) (cadr (jugadas matriz cont)))
        )
        ((and(not (null? (caddr (jugadas matriz cont)))) (not(= (verDDI (matdiag (falsaTraspuesta matriz)) (caddr (jugadas matriz cont))) -1)))
            (verDDI (matdiag (falsaTraspuesta matriz)) (caddr (jugadas matriz cont)))
        )
        ((and (not (null? (cadddr (jugadas matriz cont)))) (not(= (verVert (vertical matriz) (cadddr (jugadas matriz cont))) -1)))
            (verVert (vertical matriz) (cadddr (jugadas matriz cont)))
        )
        (else 
        (verSoluc matriz (- cont 1))
    )
))
;;Funcion soluc, diseñada para ser llamada desde la interfaz, se ingresa la matriz y un entero que indica la columna 
(define (soluc matriz)
    (cond 
    ((null? (jugadas matriz 3))
        (cond 
        ((null? (jugadas matriz 2))
            (cond 
            ((null? (jugadas matriz 1))
                (random (length (car matriz))))
            (else
                (verSoluc matriz 1)
            )))
            (else
                (verSoluc matriz 2)
            )))
    (else
    (verSoluc matriz 3)))
)

(soluc '(
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0)
)
)