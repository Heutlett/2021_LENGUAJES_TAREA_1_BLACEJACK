#lang racket
; Interfaz

(require racket/gui)
(require racket/include)

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

; Crea el marco principal
(define frame (new frame%
                   [label "BlaCE Jack"]
                   [width 900]
                   [height 600]))


; Texto que cambia según el evento(Para realizar pruebas)
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

;=========================PANELES=====================================

; Crea panel que contiene el sitio de las cartas
(define panel-crupier (new horizontal-panel% [parent frame]))

(define panel-crupierL (new vertical-panel% [parent panel-crupier]))
(define panel-crupierM (new vertical-panel% [parent panel-crupier]))
(define panel-crupierR (new vertical-panel% [parent panel-crupier]))

; Crea panel que contiene el sitio de las cartas
(define panel-juego-medio (new horizontal-panel% [parent frame]))

(define panel-juego-medioL (new vertical-panel% [parent panel-juego-medio]))
(define panel-juego-medioM (new vertical-panel% [parent panel-juego-medio]))
(define panel-juego-medioR (new vertical-panel% [parent panel-juego-medio]))

; Crea panel que contiene el sitio de las cartas
(define panel-juego-bajo (new horizontal-panel% [parent frame]))

(define panel-juego-bajoL (new vertical-panel% [parent panel-juego-bajo]))
(define panel-juego-bajoM (new vertical-panel% [parent panel-juego-bajo]))
(define panel-juego-bajoR (new vertical-panel% [parent panel-juego-bajo]))

; Crea panel que contiene a los botones "Pedir" y "Plantarse"
(define panel-botones (new horizontal-panel%
                           [parent frame]
                           [stretchable-height #f]
                           [alignment '(center center)]))

;-------------------------------------------------------------------------

;===================BOTONES====================================

; Crea botón "PEDIR"
(new button% [parent panel-botones]
             [label "Pedir"]
             [callback (lambda (button event)
                         (send msg set-label "PIDIÓ CARTA"))])

; Crea botón "PLANTARSE"
(new button% [parent panel-botones]
             [label "Plantarse"]
             [callback (lambda (button event)
                         (send msg set-label "SE PLANTÓ"))])
;-------------------------------------------------------------------

;==========================ESPACIOS DE JUEGO==========================
; Crea espacio de juego crupier
(define msgCrupier (new message% [parent panel-crupierM]
                          [label "Crupier"]))

(new canvas% [parent panel-crupierM]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "dark green")
                (send dc draw-text "Crupier" 0 0))])

; Crea espacio de juegador1
(define msgJugador1 (new message% [parent panel-juego-medioL]
                          [label "JUgador1"]))

(new canvas% [parent panel-juego-medioL]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "dark green")
                (send dc draw-text "Jugador 1" 0 0))])

; Crea espacio de juegador2
(define msgJugador2 (new message% [parent panel-juego-bajoM]
                          [label "Jugador2"]))

(new canvas% [parent panel-juego-bajoM]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "dark green")
                (send dc draw-text "Jugador 2" 0 0))])

; Crea espacio de juegador3
(define msgJugador3 (new message% [parent panel-juego-medioR]
                          [label "Jugador3"]))

(new canvas% [parent panel-juego-medioR]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "dark green")
                (send dc draw-text "Jugador 3" 0 0))])
;----------------------------------------------------------------

 
; Método que muestra el marco
(send frame show #t)

;***********************************************************