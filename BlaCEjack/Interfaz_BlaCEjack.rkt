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



;; Variable que guarda el estado del turno actual

(define turno 0)

;; Aumenta el turno en 1
(define (aumentaTurno)
  (set! turno (+ 1 turno)))

   
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


(define (iniciarJuego)
  (aumentaTurno))


;(actualizarListaJugadores( cdr(drawCard nombreJugador listaJugadores mazo )))

(define (pedirCarta jugador listaJugadores mazo)
  (cond ((equal? #t (car(drawCard jugador listaJugadores mazo )))
          (actualizarListaJugadores(cdr(drawCard jugador listaJugadores mazo ))))
        (else
         (and (actualizarListaJugadores(cdr(drawCard jugador listaJugadores mazo ))) (aumentaTurno)  ))))
          

;"Pedir cartas jugador 3"
;'(#t (("Player3" 13 ((A 1) (A 7) (A 5)))))


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