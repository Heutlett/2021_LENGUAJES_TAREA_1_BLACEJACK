#lang racket
; Interfaz

(require racket/gui)
(require racket/include)

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

;; Aumenta el turno en 1
(define (aumentaTurno)
  (set! turno (+ 1 turno)))


;; Establecer turno crupier
(define (establerTurnoCrupier)
  (set! turno 4))
   
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
            (winners? listaJugadores crupier))
           ((and (>= (cadr crupier) 16))   
            (winners? listaJugadores crupier))))))

 (define (verifica-turno)
    (cond ((or (and (= turno 2) (= (length listaJugadores) 1)) (and (= turno 3) (= (length listaJugadores) 2)) )
         (and (establerTurnoCrupier) (boton-pedir listaJugadores mazo) ))))


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
  (aumentaTurno))))

; #############################################################################################
; #############################################################################################

; PRUEBAS

; #############################################################################################
; #############################################################################################

(actualizarListaJugadores (bCEj 2))
listaJugadores
crupier
"Se barajan las cartas"
(setDeck)
(actualizarMazo (shuffle mazo))
turno
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

(iniciarJuego)

"Empieza la partida y los jugadores piden cartas hasta que llega el turno del crupier"

(boton-pedir listaJugadores mazo)
listaJugadores
crupier
turno



