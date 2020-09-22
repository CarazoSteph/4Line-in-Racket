#lang racket

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
    (append (matdiagL matriz)(matdiagR matriz))
)
;;Retorna la falsaTraspuesta de una matriz
;;Se llama falsa traspuesta debido a que invierte el orden de la misma para poder analizar las diagonales de la matriz
(define (falsaTraspuesta matriz)
    (cond ((null? (car matriz))
    '())
    (else
        (cons (sacIniMat (reverse matriz)) (falsaTraspuesta (elIniMat matriz)))))
)

;;(matdiag '((1 2 3 4 5) (2 3 4 5 6) (3 4 5 6 7) (4 5 6 7 8) (5 6 7 8 9)))
;;(matdiag  '((1 2 3) (3 4 5) (5 6 7) (7 8 9)))
;;(matdiag '((1 2 3 4) (2 3 4 5) (3 4 5 7)))
;;(falsaTraspuesta '((1 2 3 4) (2 3 4 5) (3 4 5 7)))
;;(matdiag (falsaTraspuesta '((1 2 3 4) (2 3 4 5) (3 4 5 7))))