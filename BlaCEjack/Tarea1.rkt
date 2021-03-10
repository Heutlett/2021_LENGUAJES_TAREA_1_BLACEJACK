#lang racket
;; Primer tarea Lenguajes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCIONES AUXILIARES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funcion tamano-lista
;; Recibe una lista y devuelve su tamano
;; Entradas: lista
;; Salida : tamaño de la lista

(define (tamano-lista lista)
  (cond ((list? lista)
        (tamano-lista-aux 0 lista))
        (else
         '(Error: Se requiere una lista como parametro))))

(define (tamano-lista-aux x lista)
  (cond ((null? lista)
         x)
        (else
          (tamano-lista-aux (+ x 1) (cdr lista)))))

'(Prueba tamaño lista)
(tamano-lista '(a b c))
(tamano-lista '(b c d b))


;; Funcion miembro
;; Recibe un elemento y una lista y busca
;; este elemento en la lista, devuelve #t
;; si lo encuentra y #f si no.
;; Entradas: x, lista
;; Salida : bool

(define (miembro? x lista)
  (cond ((list? lista)
        (miembro-aux x lista))
        (else
         '(Error: se necesita una lista como argumento))))

(define (miembro-aux x lista)
  (cond ((null? lista)
         #f)
        ((equal? x (car lista))
         #t)
        (else
          (miembro-aux x (cdr lista)))))

'(Prueba miembro)
(miembro? 'a '(a b c))
(miembro? 'a '(b c d))

;; Funcion eliminar
;; Recibe un elemento y una lista y busca
;; este elemento en la lista y lo borra
;; Entradas: x, lista
;; Salida: lista sin elemento eliminado

(define (eliminar x lista)
  (cond ((miembro? x lista)
         (eliminar-aux x lista '()))
        (else
         lista)))

(define (eliminar-aux x lista listaFinal)
  (cond ((null? lista)
         (ordena-final listaFinal '()))
        ((equal? x (car lista))
          (eliminar-aux x (cdr lista) listaFinal))
        (else
          (eliminar-aux x (cdr lista) (cons (car lista) listaFinal))
          )))

(define (ordena-final lista listaFinal)
  (cond ((null? lista)
         listaFinal)
        (else
         (ordena-final (cdr lista) (cons (car lista) listaFinal)))))

'(prueba eliminar)
(eliminar 'a '(a b c))
(eliminar 'a '(b c d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funciones disponibles del lenguaje:
;; Length : Devuelve el tamaño de una lista
;; Shuffle : Desordena una lista

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Se define una lista que guarda las cartas en orden
;; C : Corazones
;; D : Diamantes
;; P : Picas
;; T : Treboles
;; 1 : Az
;; 11 : J
;; 12 : Q
;; 13 : K
(define mazo '(C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13
                 D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 
                 P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P11 P12 P13
                 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13))


;; Funcion bCEj
;; Inicia el juego con la cantidad de jugadores que se ingresen como parametro
;; Entradas: x = numero de jugadores
;; Salida: inicia el juego

(define (bCEj X)
  (cond ((integer? X)
         (cond ((= 1 X)
               X)
         ((= 2 X)
               X)
         ((= 3 X)
               X)
         (else
          #f)))
        (else
         '(Ha ingresado un valor incorrecto, por favor ingrese un valor del 1 al 3))))












