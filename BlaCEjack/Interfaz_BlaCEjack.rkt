#lang racket
; Interfaz

(require racket/gui)
(require racket/include)
(require (prefix-in htdp: 2htdp/image))

(require "Logica_BlaCEjack.rkt")

;; ############################################################################
;; Variables de la interfaz
;; ############################################################################

;; Se define una lista que guarda las cartas en orden
;; C : Corazones
;; D : Diamantes
;; P : Picas
;; T : Treboles
;; 1 : Az
;; 11 : J
;; 12 : Q
;; 13 : K

;; Variable que guarda el estado del turno actual

(define turno 0)

(define espacios-fila 4)

;; Aumenta el turno en 1
(define (aumentaTurno)
  (set! turno (+ 1 turno))
  (set! espacios-fila 5))

   
(define mazo '())

;; Funcion que acomoda el maso con todas las cartas.
(define (setDeck)
  (set! mazo '((C 1) (C 2) (C 3) (C 4) (C 5) (C 6) (C 7) (C 8) (C 9) (C 10) (C 11) (C 12) (C 13)
                 (D 1) (D 2) (D 3) (D 4) (D 5) (D 6) (D 7) (D 8) (D 9) (D 10) (D 11) (D 12) (D 13) 
                 (P 1) (P 2) (P 3) (P 4) (P 5) (P 6) (P 7) (P 8) (P 9) (P 10) (P 11) (P 12) (P 13)
                 (T 1) (T 2) (T 3) (T 4) (T 5) (T 6) (T 7) (T 8) (T 9) (T 10) (T 11) (T 12) (T 13))))

;; Funcion que actualiza el mazo a medida que se avanza en el juego.
(define (actualizarMazo nuevoMazo)
  (set! mazo nuevoMazo))

;; Crupier del juego actual

(define crupier (crea-crupier))

; Función que actualiza el crupier al momento actual

(define (actualizarCrupier nuevoCrupier)
    (set! crupier nuevoCrupier)
)

; Lista de jugadores del juego actual
(define listaJugadores '())

; Función que actualiza la lista de jugadores del juego actual

(define (actualizarListaJugadores lista)
    (set! listaJugadores lista)
)

;; Definir final
(define (establerTurnoFinal)
  (set! turno 5))

;; Establecer turno crupier
(define (establerTurnoCrupier)
  (set! turno 4))

; Función para recorrer una lista
(define (recorrer n l)
  (cond ( (null? l)
          #f)
        ( (= n 0)
          (car l))
        ( else
          (recorrer (- n 1) (cdr l)))))

; #############################################################################################
; #############################################################################################

; FUNCIONES DE CONTROL DE LA INTERFAZ

; #############################################################################################
; #############################################################################################


; Define el inicio del juego asignando a turno el valor de 1, esto indica que es el turno del
; jugador1.

(define (iniciarJuego)
  (aumentaTurno))

; Funcion: boton-pedir
; Esta funcion es la accion del boton pedir en la interfaz, pedirá una carta para el jugador
; que corresponde segun el turno actual, en caso de que el jugador que pide la carta sume una
; puntuacion mayor a 21 con sus cartas volteadas se lanzará un aviso de que ha perdido y se
; aumentará en 1 el valor del turno, haciendo que continue el siguiente jugador.

;(establerTurnoFinal)
;(winners? (updateAllPlayers listaJugadores) (updateScore crupier))

(define (boton-pedir listaJugadores mazo)
  (cond ((= turno 1)
         (pedirCartaJugador (car listaJugadores) listaJugadores mazo))
        ((and (= turno 2) (>= (length listaJugadores) 2))   
         (pedirCartaJugador (cadr listaJugadores) listaJugadores mazo))
        ((and (= turno 3) (>= (length listaJugadores) 3))   
         (pedirCartaJugador (caddr listaJugadores) listaJugadores mazo))
        ((= turno 4)
         (cond ((and (not (equal? (cadr crupier) "Black-Jack")) (<= (cadr crupier) 16) )   
            (and (pedirCartaCrupier crupier mazo) (boton-pedir listaJugadores mazo)))
           ((equal? (cadr crupier) "Black-Jack")   
            (establerTurnoFinal))
           ((and (>= (cadr crupier) 16))   
            (establerTurnoFinal))))))

(define (verifica-turno)
  (cond ((or (and (= turno 2) (= (length listaJugadores) 1)) (and (= turno 3) (= (length listaJugadores) 2)) )
         (and (establerTurnoCrupier) (boton-pedir listaJugadores mazo) ))))

(define (mostrar-ganadores)
  (cond ((= turno 5)
  (winners? (updateAllPlayers listaJugadores) (updateScore crupier)))))

(define (pedirCartaCrupier crupier mazo)
  (and (actualizarMazo (cdr mazo)) (actualizarCrupier (turno-crupier crupier  mazo))))


(define (pedirCartaJugador jugador listaJugadores mazo)
  (cond ((equal? #t (car(drawCard jugador listaJugadores mazo )))
          (and (actualizarMazo (cdr mazo)) (actualizarListaJugadores(cadr(drawCard jugador listaJugadores mazo )))))
        (else
         (and (actualizarMazo (cdr mazo)) (actualizarListaJugadores(cadr(drawCard jugador listaJugadores mazo ))) (and (aumentaTurno) )  (verifica-turno)))))

; Funcion boton-plantarse

(define (boton-plantarse)
  (cond ((and (= turno 1) (= (length listaJugadores) 1)
   (establerTurnoCrupier)))
  ((and (= turno 2) (= (length listaJugadores) 2))   
   (establerTurnoCrupier))
  ((and (= turno 3) (= (length listaJugadores) 3))   
   (establerTurnoCrupier))
  (else
  (aumentaTurno)
  (set! espacios-fila 4))))

; #############################################################################################
; #############################################################################################


;Pruebas

(iniciarJuego)


"Empieza la partida y los jugadores piden cartas hasta que llega el turno del crupier"






;*************************GUI*******************************



; Crea el marco de juego
(define frame (new frame%
                   [label "BlaCE Jack"]
                   [width 900]
                   [height 800]))

; Crea el marco principal
(define Firstframe (new frame%
                   [label "BlaCE Jack"]
                   [width 400]
                   [height 200]
                   ))

; Texto que cambia según el evento(Para realizar pruebas)
(define msg (new message% [parent frame]
                          [label "No events so far..."]))


;=========================PANELES=====================================

; Crea panel que contiene el seleccionador de jugadores
(define panel-label-cantidad (new horizontal-panel%
                                  [parent Firstframe]
                                  [alignment '(center center)]))
(define panel-botones-cantidad (new horizontal-panel%
                                    [parent Firstframe]
                                    [alignment '(center center)]))

(define cantidad (new message% [parent panel-label-cantidad]
                          [label "Seleccione la cantidad de Jugadores"]))

; Crea panel que contiene el sitio de las cartas del crupier
(define panel-juego-alto (new horizontal-panel% [parent frame] [min-height 200]))

(define panel-crupierL (new vertical-panel% [parent panel-juego-alto] [min-width 200]))
(define panel-crupierM (new vertical-panel% [parent panel-juego-alto]))
(define panel-crupierR (new vertical-panel% [parent panel-juego-alto] [min-width 200]))

; Crea panel que contiene el sitio de las cartas del jugador 1 y 3
(define panel-juego-medio (new horizontal-panel% [parent frame] [min-height 200]))

(define panel-juego-medioL (new vertical-panel% [parent panel-juego-medio]))
(define panel-juego-medioM (new vertical-panel% [parent panel-juego-medio] [min-width 200]))
(define panel-juego-medioR (new vertical-panel% [parent panel-juego-medio]))

; Crea panel que contiene el sitio de las cartas del jugador 2
(define panel-juego-bajo (new horizontal-panel% [parent frame] [min-height 200]))

(define panel-juego-bajoL (new vertical-panel% [parent panel-juego-bajo] [min-width 200]))
(define panel-juego-bajoM (new vertical-panel% [parent panel-juego-bajo]))
(define panel-juego-bajoR (new vertical-panel% [parent panel-juego-bajo] [min-width 200]))

; Crea panel que contiene a los botones "Pedir" y "Plantarse"
(define panel-display (new horizontal-panel%
                           [parent frame]
                           [stretchable-height #f]))

(define panel-info (new vertical-panel%
                           [parent panel-display]
                           [alignment '(center center)]))

(define panel-botones (new vertical-panel%
                           [parent panel-display]
                           [alignment '(center center)]))


;-------------------------------------------------------------------------

;===================BOTONES====================================

; Crea botón "1"
(new button% [parent panel-botones-cantidad]
             [label "1"]
             [callback (lambda (button event)
                         
                         (actualizarListaJugadores (bCEj 1))
                         (setDeck)
                         (actualizarMazo (shuffle mazo))

                         (actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
                         (actualizarMazo (cdddr mazo))
                         (actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
                         (actualizarMazo (cdddr mazo))

                         (actualizarCrupier (dar-carta crupier  mazo))
                         (actualizarMazo (cdr mazo))
                         (actualizarCrupier (dar-carta crupier  mazo))
                         (actualizarMazo (cdr mazo))

                         (primerasCartas listaJugadores crupier)

                         listaJugadores
                         crupier
                         
                         (send Firstframe show #f)
                         (send frame show #t))])
; Crea botón "2"
(new button% [parent panel-botones-cantidad]
             [label "2"]
             [callback (lambda (button event)
                         
                         (actualizarListaJugadores (bCEj 2))
                         (setDeck)
                         (actualizarMazo (shuffle mazo))

                         (actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
                         (actualizarMazo (cdddr mazo))
                         (actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
                         (actualizarMazo (cdddr mazo))

                         (actualizarCrupier (dar-carta crupier  mazo))
                         (actualizarMazo (cdr mazo))
                         (actualizarCrupier (dar-carta crupier  mazo))
                         (actualizarMazo (cdr mazo))

                         (primerasCartas listaJugadores crupier)

                         listaJugadores
                         crupier
                         
                         (send Firstframe show #f)
                         (send frame show #t))])
; Crea botón "3"
(new button% [parent panel-botones-cantidad]
             [label "3"]
             [callback (lambda (button event)
                         
                         (actualizarListaJugadores (bCEj 3))
                         (setDeck)
                         (actualizarMazo (shuffle mazo))

                         (actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
                         (actualizarMazo (cdddr mazo))
                         (actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
                         (actualizarMazo (cdddr mazo))

                         (actualizarCrupier (dar-carta crupier  mazo))
                         (actualizarMazo (cdr mazo))
                         (actualizarCrupier (dar-carta crupier  mazo))
                         (actualizarMazo (cdr mazo))

                         (primerasCartas listaJugadores crupier)

                         listaJugadores
                         crupier
                         
                         (send Firstframe show #f)
                         (send frame show #t))])


; Crea botón "PEDIR"
(new button% [parent panel-botones]
             [label "Pedir"]
             [callback (lambda (button event)
                         (send msg set-label "PIDIÓ CARTA")
                         (boton-pedir listaJugadores mazo)
                         (dibujarCartas turno c))])

; Crea botón "PLANTARSE"
(new button% [parent panel-botones]
             [label "Plantarse"]
             [callback (lambda (button event)
                         (send msg set-label "SE PLANTÓ")
                         (boton-plantarse))])
;-------------------------------------------------------------------

;==========================ESPACIOS DE JUEGO==========================
; Crea espacio de juego crupier
(define panel-crupier (new horizontal-panel%
                           [parent panel-crupierM]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-crupier-juego1 (new horizontal-panel% [parent panel-crupierM] [style '(border)]))
(define panel-crupier-juego2 (new horizontal-panel% [parent panel-crupierM] [style '(border)]))

(define msgCrupier (new message% [parent panel-crupier]
                          [label "Crupier"]))


; Crea espacio de jugador1
(define panel-jugador1 (new horizontal-panel%
                           [parent panel-juego-medioL]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-jugador1-juego1 (new horizontal-panel% [parent panel-juego-medioL] [style '(border)]))
(define panel-jugador1-juego2 (new horizontal-panel% [parent panel-juego-medioL] [style '(border)]))

(define msgJugador1 (new message% [parent panel-juego-medioL]
                          [label "Jugador1"]))


; Crea espacio de jugador2
(define panel-jugador2 (new horizontal-panel%
                           [parent panel-juego-bajoM]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-jugador2-juego1 (new horizontal-panel% [parent panel-juego-bajoM] [style '(border)]))
(define panel-jugador2-juego2 (new horizontal-panel% [parent panel-juego-bajoM] [style '(border)]))

(define msgJugador2 (new message% [parent panel-juego-bajoM]
                          [label "Jugador2"]))


; Crea espacio de jugador3
(define panel-jugador3 (new horizontal-panel%
                           [parent panel-juego-medioR]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-jugador3-juego1 (new horizontal-panel% [parent panel-juego-medioR] [style '(border)]))
(define panel-jugador3-juego2 (new horizontal-panel% [parent panel-juego-medioR] [style '(border)]))

(define msgJugador3 (new message% [parent panel-juego-medioR]
                          [label "Jugador3"]))

;----------------------------------------------------------------

;========================Espacio para las cartas==================


;Dibuja las cartas en el espacio del jugador o crupier asignado
;con la variable espacios-fila solo permite posicionar 5 cartas en fila
(define (dibujarCartas turno carta)
  (cond ( (<= espacios-fila 0)
          (dibujarCartas-aux turno (string-append "cards/" (symbol->string (car carta)) (number->string (cadr carta)) ".png") 2))
        ( else
          (dibujarCartas-aux turno (string-append "cards/" (symbol->string (car carta)) (number->string (cadr carta)) ".png") 1)
          (set! espacios-fila (sub1 espacios-fila)))))

;Función auxiliar, comprueba el jugador y la zona en donde debe graficar
(define (dibujarCartas-aux turno carta zona)
  (cond ( (and (equal? turno 4) (equal? zona 1))
          (new message% [parent panel-crupier-juego1]
                        [label (read-bitmap carta)]))
        ( (and (equal? turno 4) (equal? zona 2))
          (new message% [parent panel-crupier-juego2]
                        [label (read-bitmap carta)]))
        ( (and (equal? turno 1) (equal? zona 1))
          (new message% [parent panel-jugador1-juego1]
                        [label (read-bitmap carta)]))
        ( (and (equal? turno 1) (equal? zona 2))
          (new message% [parent panel-jugador1-juego2]
                        [label (read-bitmap carta)]))
        ( (and (equal? turno 2) (equal? zona 1))
          (new message% [parent panel-jugador2-juego1]
                        [label (read-bitmap carta)]))
        ( (and (equal? turno 2) (equal? zona 2))
          (new message% [parent panel-jugador2-juego2]
                        [label (read-bitmap carta)]))
        ( (and (equal? turno 3) (equal? zona 1))
          (new message% [parent panel-jugador3-juego1]
                        [label (read-bitmap carta)]))
        ( (and (equal? turno 3) (equal? zona 2))
          (new message% [parent panel-jugador3-juego2]
                        [label (read-bitmap carta)]))
        ))

;Definiciones temporales para realizar pruebas
;(define t 0)
(define c '(T 12))

;Metodo llamado por plantarse para realziar pruebas
(define (pasar)
  ;(set! t (add1 t))
  (set! espacios-fila 4))


(define (primerasCartas lista crup)
  (primerasCartas-aux 0 (+ (length lista) 1) (append lista (list crup))))

(define (primerasCartas-aux t j lista)
  (cond ( (equal? j t)
          1)
        ( else
          (dibujarCartas (+ t 1) '(B 0))
          (dibujarCartas (+ t 1) (cadr (recorrer 2 (recorrer t lista))))
          (set! espacios-fila 4)
          (primerasCartas-aux (+ t 1) j lista))))
;-----------------------------------------------------------------

;======================INFORMACION================================

(new message% [parent panel-info]
                          [label "Turno de :Jugador 1"])

(new message% [parent panel-info]
                          [label "Puntaje actual: 10"])

;-----------------------------------------------------------------

 
; Método que muestra el marco
(send Firstframe show #t)

;***********************************************************