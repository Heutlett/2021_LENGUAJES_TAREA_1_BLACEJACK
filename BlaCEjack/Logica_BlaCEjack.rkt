#lang racket

;; #################################################################################
;; PROVIDES
;; #################################################################################

(provide turno-crupier)   ;; Parametros: crupierDeck, mazo
(provide bCEj)            ;; Parametros: cantidad de jugadores, de 1 a 3
(provide reparte-cartas)  ;; Parametros: lista de jugadores, mazo
(provide dar-carta)       ;; Parametros: nombre del jugador, listaDeJugadores, mazo
(provide crea-crupier)
;(provide )        ;;Parametros: lista del jugador

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


;; Funcion crea-crupier
;; Crea al crupier en su estado inicial

(define (crea-crupier)
  ' ("crupier" 0 ()) )


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

;; Funcion dar-carta
;; Da una carta al jugador pasado por parametro y devuelve ese mismo jugador modificado con su nueva carta.
;; Entradas: jugador de la forma: ( "nombre" puntuacion '(lista cartas) )
;; Salida: el mismo jugador pero con la nueva carta = ( "nombre" puntuacion '(lista cartas + carta nueva) )
(define (dar-carta jugador mazo)
  (cond ((null? (caddr jugador))
         (append (list (car jugador) (cadr jugador)) (list (append  '( )  (list (car mazo))))    ))
        (else
  (append (list (car jugador) (cadr jugador)) (list (append  (caddr jugador)  (list (car mazo))) )    ))))

;(dar-carta '("jugador2" 0 '((A 3) (A 4))) mazo)        

;; Funcion dar-carta-jugador
;; Da una carta a un jugador y devuelve la lista con todos los jugadores y el jugador con la carta nueva actualizado
;; Entradas: jugador = nombre del jugador "jugador", listaJugadores, mazo
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

;; Funcion reparte-cartas
;; Reparte una carta a cada jugador y devuelve la lista con todos los jugadores actualizada con su carta nueva
;; cada uno
;; Entradas: listaJugadores, mazo
;; Salida: listaJugadores actualizada

(define (reparte-cartas listaJugadores mazo)
  (reparte-cartas-aux listaJugadores mazo '()))


(define (reparte-cartas-aux listaJugadores mazo resultado)
  (cond ((null? listaJugadores)
         resultado)
        (else
         (reparte-cartas-aux (cdr listaJugadores) (cdr mazo) (append resultado (list (dar-carta (car listaJugadores) mazo)))   ))))

;(reparte-cartas '(("jugador1" 0 ()) ("jugador2" 0 ()) ("jugador3" 0 ())) mazo)

;(reparte-cartas '(("jugador1" 0 ((P 5))) ("jugador2" 0 ((C 12))) ("jugador3" 0 ((D 8)))) mazo)

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

;; getPlayerName
;; Retorna el nombre del jugador.
;; Input: player - jugador.
;; Output: nombre del jugador.
(define (getPlayerName player)
  (car player))

;; getPlayerScore
;; Retorna el puntaje actual del jugador.
;; Input: player - jugador.
;; Output: puntaje del jugador.
(define (getPlayerScore player)
  (cadr player))

;; getPlayerDeck
;; Retorna el mazo de cartas del jugador.
;; Input: player - jugador.
;; Output: mazo de cartas del jugador.
(define (getPlayerDeck player)
  (caddr player))


;; getCardsTotalValue
;; Retorna la suma total de todas las cartas del jugador,tomando en cuenta todas las particularidades del juego.
;; Input: playerCards - cartas del jugador.
;; Output: suma total de las cartas.
(define (getCardsTotalValue playerCards)
  (cond [(null? playerCards) 0]
        [(blackjack? playerCards) "Black-Jack"]
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
  (cond ((equal? (getCardsTotalValue crupierDeck) "Black-Jack")
         (crupierDeck ))
        ((<= (getCardsTotalValue crupierDeck) 16)
         (dar-carta crupierDeck mazo))
        (else
         crupierDeck)))

;; updateScores
;; Actualiza los puntajes de los jugadores y el crupier.
;; Input: player - jugador.
;; Output: lista correspondiente al jugador con su puntuación actualizada.
(define (updateScore player)
  (list (getPlayerName player) (getCardsTotalValue (getPlayerDeck player)) (getPlayerDeck player)))

;; updatePlayer
;; Función que recibe un jugador y retorna la lista de jugadores con el jugador actualizado.
;; Input: player - jugador a actualizar, playersList - listajugadores a actualizar.
;; Output: lista de jugadores actualizada.
(define (updatePlayerInList player playersList)
  (cond ((null? playersList) #f)
        (else (updatePlayer_aux player playersList '()))))
(define (updatePlayer_aux player playersList updated)
  (cond ((null? playersList) #f)
        ((equal? (getPlayerName player) (getPlayerName (car playersList))) (append updated (list player)))
        (else (updatePlayer_aux player (cdr playersList) updated))))

;; visibleDeck
;; Retorna la puntuación de las visibles del jugador.
;; Input: player
;; Output: puntuacion visible del jugador.
(define (visibleDeck player)
  (getCardsTotalValue (cdr (getPlayerDeck player))))

;; drawCard
;; Funcion que agrega una carta al mazo del jugador recibido como parametro.
;; Input: player - jugador, playersList - lista de jugadores, mazo.
;; Output: bool, si puede seguir pidiendo, tira verdadero y si no, tira false. Ademas de la lista de jugadores actualizada.
;(define (drawCard player playersList deck)
;  ;una vez dada una carta nueva, el jugador se manda a verificar si puede pedir otra vez
;  (cond ((equal? #t (car (keepDrawing (updateScore (dar-carta player deck))))) )  
;  
;  (keepDrawing (updatePlayer (updateScore (dar-carta player deck)) playersList))
 ; 
 ; (cond ((not (equal? "No hay jugadores" (updatePlayer (updateScore (dar-carta player deck)) playersList))) (keepDrawing


;(define (drawCard_Aux player playersList)
 ; (updatePlayer (updateScore player))
;  (cond

;; keepDrawing
;; Función que verifica si las cartas visibles del jugador suman o se pasan de 21.
;; Input: player - jugador.
;; Output: #f si no se pasa, #t si es 21 o se pasa.
(define (keepDrawing player)
  (cond [(<= 21 [getCardsTotalValue (cdr (getPlayerDeck player))]) (list #t player)]
        [else ((list #f player))]))

;; winners?
;; Retorna el ganador o los ganadores de la ronda.
;; Input: playersList - lista de jugadores, crupier - casa.
;; Output: lista con los ganadores de la ronda.
(define (winners? playersList crupier)
  (cond ((null? playersList) "No hay jugadores")
        [(equal? "Black-Jack" (getPlayerScore crupier)) (~a (getPlayerName crupier) " gana con " (getPlayerScore crupier))] ;si la casa tiene blackjack, gana la casa.
        (else (winners?_aux playersList crupier '()))))

(define (winners?_aux playersList crupier winnersList)
  (cond [(and (null? playersList) (null? winnersList)) (~a (getPlayerName crupier) " gana con " (getPlayerScore crupier))]
        [(and (null? playersList) (not (null? winnersList))) winnersList] ;(~a "Gana(n) " winnersList)]
        [else (winners?_aux (cdr playersList) crupier [append winnersList (victoryCondition (car playersList) crupier)]
   )]))                                            ;; (append '() '(("Player1" 10) ("Player2" 20)))

;; victoryCondition
;; Compara las puntuaciones y retorna una tupla con el nombre del jugador y el puntaje, si este le gana al crupier.
;; Input: player - jugador, crupier - casa.
;; Output: Lista con tupla con el nombre del jugador y el puntaje, si gana. Nada, si pierde.
(define (victoryCondition player crupier)
  (cond
    [(equal? "Black-Jack" (getPlayerScore player)) (list (list (getPlayerName player) (getPlayerScore player)))] ;si el jugaodr tiene blackjack, gana.
    [(< 21 (getPlayerScore player)) '()] ;si se pasa de 21, pierde.
    [(< (getPlayerScore crupier) (getPlayerScore player)) (list (list (getPlayerName player) (getPlayerScore player)))] ;si tiene mas que el crupier, gana.
    [else '()]))



;-------------------Pruebas-------------------

;(reparte-cartas '(( (A 1) (A 5)) ((A 2) (D 1)) ((A 3) (P 4))) (shuffle mazo) )

;(dar-carta '("crupier" 0 ()) mazo)        

;(blackjack? '((C 1) (P 11)))
;(blackjack? '((A 12) (A 1)))
;(blackjack? '((A 13) (A 13)))

;(checkCardValue 11 0)
;(checkCardValue 1 10)
;(checkCardValue 1 11)
;(checkCardValue 3 10)

;(getPlayerScore (car listaPlayers))
;(getPlayerDeck (car listaPlayers))

;(getCardsTotalValue '((C 1) (A 5) (C 1))) ;17
;(getCardsTotalValue '((C 1) (A 13) (C 1))) ;22
;(getCardsTotalValue '((C 11) (A 1))) ;BlackJack
;(getCardsTotalValue '((D 12) (A 13) (C 1))) ;21
;(getCardsTotalValue '((D 1) (D 1) (D 1))) ;13

;(turno-crupier '((C 1) (A 5) (C 1)) (shuffle mazo))
;(turno-crupier '((C 1) (A 2) (P 1)) (shuffle mazo))
;(turno-crupier '((C 1) (A 2) (P 1) (H 1)) (shuffle mazo))
;(turno-crupier '((C 1) (A 9)) (shuffle mazo))


(define deck '((T 1) (D 2) (T 3) (C 4) (A 5)))
(define listPlayers  '(("Player1" 0 ((A 1) (C 12) (D 12)))
                       ("Player2" 0 ((A 1) (C 5) (D 6)))
                       ("Player3" 0 ((A 2) (A 1) (C 8)))))
(define crupier  '("Crupier" "Black-Jack" ((D 11) (C 9))))

;(victoryCondition (cadr listPlayers) crupier)
;(winners? listPlayers crupier)

;(visibleDeck (car listPlayers))
;(visibleDeck (cadr listPlayers))
;(visibleDeck (caddr listPlayers))

;(drawCard (car listPlayers) listPlayers deck)
          
;(updateScore '("Player1" 0 ((A 1) (C 12))))
;(updateScore '("Player2" 0 ((A 2) (C 5))))
;(updateScore '("Player3" 0 ((A 3) (C 12))))
