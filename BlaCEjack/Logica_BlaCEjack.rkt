#lang racket

;; #################################################################################
;; PROVIDES
;; #################################################################################

(provide turno-crupier)   ;; Parametros: crupierDeck, mazo
(provide bCEj)            ;; Parametros: cantidad de jugadores, de 1 a 3
(provide reparte-cartas)  ;; Parametros: lista de jugadores, mazo
(provide dar-carta-jugador) ;; ;; Parametros: nombre del jugador, listaDeJugadores, mazo

;; #################################################################################
;; #################################################################################



;; #################################################################################
;; FUNCIONES AUXILIARES
;; #################################################################################

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
               '( ("jugador1" 0 ()) ))
         ((= 2 X)
               '( ("jugador1" 0 ()) ("jugador2" 0 ())))
         ((= 3 X)
               '( ("jugador1" 0 ()) ("jugador2" 0 ()) ("jugador3" 0 ())))
         (else
          #f)))
        (else
         '(Ha ingresado un valor incorrecto, por favor ingrese un valor del 1 al 3))))


;; Estructura jugador:    ( "nombre" puntuacion '(lista cartas) )


;; respaldo dar car
; (define (dar-carta jugador mazo)
;  (append (list (car jugador) (cadr jugador)) (list (append  (car (cdaddr jugador     ))  (list (car mazo))) )    ))

;; Dar una carta al jugador pasado por parametro y devuelve ese mismo jugador modificado
(define (dar-carta jugador mazo)
  (cond ((null? (caddr jugador))
         (append (list (car jugador) (cadr jugador)) (list (append  '( )  (list (car mazo))))    ))
        (else
  (append (list (car jugador) (cadr jugador)) (list (append  (caddr jugador)  (list (car mazo))) )    ))))

;(dar-carta '("jugador2" 0 '((A 3) (A 4))) mazo)        

;; Da una carta a un jugador y devuelve la lista de jugadores actualizada
(define (dar-carta-jugador jugador listaJugadores mazo)
  (dar-carta-jugador-aux jugador listaJugadores mazo '()))

(define (dar-carta-jugador-aux jugador listaJugadores mazo resultado)
  (cond  ((null? listaJugadores)
         resultado)
         ((equal? jugador (caar listaJugadores))
         (dar-carta-jugador-aux jugador (cdr listaJugadores) mazo (append resultado (list (dar-carta (car listaJugadores) mazo)))))
        
        (else
         (dar-carta-jugador-aux jugador (cdr listaJugadores) mazo (append resultado (list(car listaJugadores)))))))

;(dar-carta-jugador "jugador1" '(("jugador1" 0 '((A 1) (A 3))) ("jugador2" 0 '()) ("jugador3" 0 '())) mazo)
 
;; Reparte una carta a cada jugador
(define (reparte-cartas listaJugadores mazo)
  (reparte-cartas-aux listaJugadores mazo '()))


(define (reparte-cartas-aux listaJugadores mazo resultado)
  (cond ((null? listaJugadores)
         resultado)
        (else
         (reparte-cartas-aux (cdr listaJugadores) (cdr mazo) (append resultado (list (dar-carta (car listaJugadores) mazo)))   ))))

"Prueba logica"
(reparte-cartas '(("jugador1" 0 ()) ("jugador2" 0 ()) ("jugador3" 0 ())) mazo)
"Prueba logica"

"Prueba interfaz"
(reparte-cartas '(("jugador1" 0 ((P 5))) ("jugador2" 0 ((C 12))) ("jugador3" 0 ((D 8)))) mazo)
"Prueba interfaz"

;; getLetter
;; Retorna la letra del par ordenada de la carta.
;; Input: pair - par con la letra y el valor.
;; Output: letra del par.
(define (getLetter pair)
  (car pair))

;; getValue
;; Retorna la numero del par ordenada de la carta.
;; Input: pair - par con la letra y el valor.
;; Output: valor del par.
(define (getValue pair)
  (cadr pair))

;; getCardsTotalValue
;; Retorna la suma total de todas las cartas del jugador,tomando en cuenta todas las particularidades del juego.
;; Input: playerCards - cartas del jugador.
;; Output: suma total de las cartas.
(define (getCardsTotalValue playerCards)
  (cond [(null? playerCards) 0]
        [(blackjack? playerCards) "BlackJack-21"]
        [else (cardsTotalValue_aux playerCards 0)]))
(define (cardsTotalValue_aux playerCards total)
  (cond [(null? playerCards) total]
        [else ;recorre la lista de cartas y suma al total el valor de la carta actual.
         (cardsTotalValue_aux (cdr playerCards) (+ total (checkCardValue (getValue (car playerCards)) total)))]))

;; blackjack?
;; Verifica si la suma de las cartas de un jugador es blackjack.
;; Input: playerCards - cartas del jugador.
;; Output: #t si se cumple BlackJack, #f si no.
(define (blackjack? playerCards)
  (cond [(not (= 2 (length playerCards))) #f]
        ;si la primera carta es K-Q-J y la segunda un As -> #t
        [(and (<= 10 (getValue(car playerCards))) (= 1 (getValue (cadr playerCards)))) #t]
        ;si la primera carta es un As y la segunda K-Q-J. -> #t
        [(and (= 1 (getValue(car playerCards))) (<= 10 (getValue (cadr playerCards)))) #t]
        [else #f]))

;; checkCardValue
;; Retorna el valor de la carta, ya sea si es un As (analizando el puntaje actual), una letra o un valor normal.
;; Input: playerCards - cartas del jugador.
;; Output: valor de la carta.
(define (checkCardValue value total)
  (cond [(< 10 value) 10] ;(K-J-Q) - valen 10
        [(and (= 1 value) (>= 21 (+ total 11))) 11] ;As - vale 11 (cuando si se suma es igual o menor a 21).
        [else value])) ;(2-3-4-5-6-7-8-9-10) - valen su normal.


;; Turno crupier
;; Analiza si la puntuacion del crupier es menor a 16 para solicitar otra carta
;; o de lo contrario plantarse

(define (turno-crupier crupierDeck mazo)
  (cond ((equal? (getCardsTotalValue crupierDeck) "BlackJack-21")
         (crupierDeck ))
        ((<= (getCardsTotalValue crupierDeck) 16)
         (dar-carta crupierDeck mazo))
        (else
         crupierDeck)))

;-------------------Pruebas-------------------

;(reparte-cartas '(( (A 1) (A 5)) ((A 2) (D 1)) ((A 3) (P 4))) (shuffle mazo) )



;(blackjack? '((C 1) (P 11)))
;(blackjack? '((A 12) (A 1)))
;(blackjack? '((A 13) (A 13)))

;(checkCardValue 11 0)
;(checkCardValue 1 10)
;(checkCardValue 1 11)
;(checkCardValue 3 10)

;(getCardsTotalValue '((C 1) (A 5) (C 1))) ;17
;(getCardsTotalValue '((C 1) (A 13) (C 1))) ;22
;(getCardsTotalValue '((C 11) (A 1))) ;BlackJack
;(getCardsTotalValue '((D 12) (A 13) (C 1))) ;21
;(getCardsTotalValue '((D 1) (D 1) (D 1))) ;13

;(turno-crupier '((C 1) (A 5) (C 1)) (shuffle mazo))
;(turno-crupier '((C 1) (A 2) (P 1)) (shuffle mazo))
;(turno-crupier '((C 1) (A 2) (P 1) (H 1)) (shuffle mazo))
;(turno-crupier '((C 1) (A 9)) (shuffle mazo))

