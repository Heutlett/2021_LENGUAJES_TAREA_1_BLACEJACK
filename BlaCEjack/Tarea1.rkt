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
(define mazo '((C 1) (C 2) (C 3) (C 4) (C 5) (C 6) (C 7) (C 8) (C 9) (C 10) (C 11) (C 12) (C 13)
                 (D 1) (D 2) (D 3) (D 4) (D 5) (D 6) (D 7) (D 8) (D 9) (D 10) (D 11) (D 12) (D 13) 
                 (P 1) (P 2) (P 3) (P 4) (P 5) (P 6) (P 7) (P 8) (P 9) (P 10) (P 11) (P 12) (P 13)
                 (T 1) (T 2) (T 3) (T 4) (T 5) (T 6) (T 7) (T 8) (T 9) (T 10) (T 11) (T 12) (T 13)))


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


;; Dar una carta al jugador pasado por parametro

(define (dar-carta jugador mazo)
  (append jugador (list (car mazo))))

 
;; Reparte una carta a cada jugador

(define (reparte-cartas listaJugadores mazo resultado)
  (cond ((null? listaJugadores)
         resultado)
        (else
         (reparte-cartas (cdr listaJugadores) (cdr mazo) (append resultado (list (dar-carta (car listaJugadores) mazo)))   ))))


;; Verifica la condicion de un jugador con respecto a su puntuacion

;;(define (verificar-condicion jugador)
;;  (


;; Calcula la puntuacion del jugador sumando carta a carta

(define (calcula-puntuacion jugador resultado)
  (cond ((null? jugador)
         resultado)
        (else
         (calcula-puntuacion (cdr jugador) (+ (cadar jugador) resultado)))))  ;; Arreglar que sume solo 10 en caso de ser J, Q, K o sea pares ordenados (X 11) (X 12) (X 13)

;; Verifica si el jugador tiene un blackjack




(reparte-cartas '(( (A 1) (A 5)) ((A 2) (D 1)) ((A 3) (P 4))) mazo '())


