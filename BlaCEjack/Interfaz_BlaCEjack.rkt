#lang racket
; Interfaz

(require racket/gui)
(require racket/include)
(require (prefix-in htdp: 2htdp/image))

(require "Logica_BlaCEjack.rkt")

;Condición de que el juego comenzó
(define gameStart #f)

; Función que dice si el juego comenzó o no

(define (asignar_gameStart bool)
    (set! gameStart (cons bool gameStart))
)

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

;; Pruebas para manejar las variables

;; listaJugadores

;; (actualizarListaJugadores '( ((A 1) (A 5)) ((A 2) (D 1)) ((A 3) (P 4)) ))

;; listaJugadores


"Simulacion juego del juego"
"Se inicia con tres jugadores sin cartas y el crupier"
(actualizarListaJugadores (bCEj 3))
listaJugadores
crupier
"Se barajan las cartas"
(setDeck)
(actualizarMazo (shuffle mazo))
"Ahora se reparten las primeras dos cartas a los jugadores"
(actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
(actualizarMazo (cdddr mazo))
(actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
(actualizarMazo (cdddr mazo))
listaJugadores
"Ahora se reparten las primeras dos cartas del crupier"

(actualizarCrupier (dar-carta crupier  mazo))
(actualizarMazo (cdr mazo))
(actualizarCrupier (dar-carta crupier  mazo))
(actualizarMazo (cdr mazo))
crupier



;crupier
;(actualizarCrupier (turno-crupier crupier (shuffle mazo)))
;crupier
;(actualizarCrupier (turno-crupier crupier (shuffle mazo)))
;crupier
;(actualizarCrupier (turno-crupier crupier (shuffle mazo)))
;crupier
;(actualizarCrupier (turno-crupier crupier (shuffle mazo)))
;crupier
;(actualizarCrupier (turno-crupier crupier (shuffle mazo)))
;crupier

;*************************GUI*******************************

(define espacios-fila 5)

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

; Crea panel que contiene el sitio de las cartas
(define panel-juego-alto (new horizontal-panel% [parent frame] [min-height 200]))

(define panel-crupierL (new vertical-panel% [parent panel-juego-alto] [min-width 200]))
(define panel-crupierM (new vertical-panel% [parent panel-juego-alto]))
(define panel-crupierR (new vertical-panel% [parent panel-juego-alto] [min-width 200]))

; Crea panel que contiene el sitio de las cartas
(define panel-juego-medio (new horizontal-panel% [parent frame] [min-height 200]))

(define panel-juego-medioL (new vertical-panel% [parent panel-juego-medio]))
(define panel-juego-medioM (new vertical-panel% [parent panel-juego-medio]))
(define panel-juego-medioR (new vertical-panel% [parent panel-juego-medio]))

; Crea panel que contiene el sitio de las cartas
(define panel-juego-bajo (new horizontal-panel% [parent frame] [min-height 200]))

(define panel-juego-bajoL (new vertical-panel% [parent panel-juego-bajo]))
(define panel-juego-bajoM (new vertical-panel% [parent panel-juego-bajo]))
(define panel-juego-bajoR (new vertical-panel% [parent panel-juego-bajo]))

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
                         (send Firstframe show #f)
                         (send frame show #t))])
; Crea botón "2"
(new button% [parent panel-botones-cantidad]
             [label "2"]
             [callback (lambda (button event)
                         (send Firstframe show #f)
                         (send frame show #t))])
; Crea botón "3"
(new button% [parent panel-botones-cantidad]
             [label "3"]
             [callback (lambda (button event)
                         (send Firstframe show #f)
                         (send frame show #t))])


; Crea botón "PEDIR"
(new button% [parent panel-botones]
             [label "Pedir"]
             [callback (lambda (button event)
                         (send msg set-label "PIDIÓ CARTA")
                         (dibujarCartas))])

; Crea botón "PLANTARSE"
(new button% [parent panel-botones]
             [label "Plantarse"]
             [callback (lambda (button event)
                         (send msg set-label "SE PLANTÓ"))])
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
(define (dibujarCartas)
  (cond ( (<= espacios-fila 0)
          (new message% [parent panel-crupier-juego2]
                          [label (read-bitmap "cards/T3.png")]))
        ( else
          (new message% [parent panel-crupier-juego1]
                          [label (read-bitmap "cards/BG.png")])
          (set! espacios-fila (sub1 espacios-fila)))))


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