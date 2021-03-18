#lang racket/base
(require racket/gui)
(require racket/include)
(require (prefix-in htdp: 2htdp/image))


(require "Logica_BlaCEjack.rkt")


; #########################################################
; #########################################################

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
            (pedirCartaCrupier crupier mazo) (actualizarMazo (cdr mazo)) (boton-pedir listaJugadores (cdr mazo)))
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
  (aumentaTurno))))

; #############################################################################################
; #############################################################################################






; #########################################################
; #########################################################

; Crea el marco de juego
(define frame (new frame%
                   [label "BlaCE Jack"]
                   [width 1200]
                   [height 800]))

; Crea el marco principal
(define Firstframe (new frame%
                   [label "BlaCE Jack"]
                   [width 400]
                   [height 200]
                   ))



; Método que muestra el marco
(send Firstframe show #t)

;***********************************************************


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
(define panel-juego-medioM (new vertical-panel% [parent panel-juego-medio] [min-width 200] [alignment '(left center)]))
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

(new message% [parent panel-juego-medioM]
                          [label (read-bitmap "cards/MAZO.png" )]
                          )

;==========================BOTONES====================================

; Crea botón "1"
(new button% [parent panel-botones-cantidad]
             [label "1"]
             [callback (lambda (button event)
                         (inicioInterfaz 1))])

; Crea botón "2"
(new button% [parent panel-botones-cantidad]
             [label "2"]
             [callback (lambda (button event)
                         (inicioInterfaz 2))])

; Crea botón "3"
(new button% [parent panel-botones-cantidad]
             [label "3"]
             [callback (lambda (button event)
                         (inicioInterfaz 3))])


;Encapsula las funciones necesarias para iniciar el juego
(define (inicioInterfaz cantidad)
  (actualizarListaJugadores (bCEj cantidad))
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

  ;Actualiza los puntajes iniciales del juego.
  (actualizarCrupier (updateScore crupier))
  (actualizarListaJugadores (updateAllPlayers listaJugadores))

  (actualizar_cartas listaJugadores)
                         
  (send Firstframe show #f)
  (send frame show #t)
  (actTurno)
  (actPuntos))

; Crea botón "PEDIR"
(new button% [parent panel-botones]
             [label "Pedir"]
             [callback (lambda (button event)
                         ;(send msg set-label "PIDIÓ CARTA")
                         (actTurno)
                         (actPuntos)
                         (boton-pedir listaJugadores mazo)
                         (actualizar_cartas listaJugadores)
                         (actTurno)
                         (actPuntos))])

; Crea botón "PLANTARSE"
(new button% [parent panel-botones]
             [label "Plantarse"]
             [callback (lambda (button event)
                         ;(send msg set-label "SE PLANTÓ")
                         (boton-plantarse)
                         (actTurno)
                         (actPuntos))])

;---------------------------------------------------------------------

;==========================ESPACIOS DE JUEGO==========================
; Crea espacio de juego crupier
(define panel-crupier (new horizontal-panel%
                           [parent panel-crupierM]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-crupier-juego1 (new horizontal-panel% [parent panel-crupierM] ))
(define panel-crupier-juego2 (new horizontal-panel% [parent panel-crupierM] ))

(define msgCrupier (new message% [parent panel-crupier]
                          [label "Crupier"]))


; Crea espacio de jugador1
(define panel-jugador1 (new horizontal-panel%
                           [parent panel-juego-medioL]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-jugador1-juego1 (new horizontal-panel% [parent panel-juego-medioL]))
(define panel-jugador1-juego2 (new horizontal-panel% [parent panel-juego-medioL]))

(define msgJugador1 (new message% [parent panel-juego-medioL]
                          [label "Jugador1"]))


; Crea espacio de jugador2
(define panel-jugador2 (new horizontal-panel%
                           [parent panel-juego-bajoM]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-jugador2-juego1 (new horizontal-panel% [parent panel-juego-bajoM] ))
(define panel-jugador2-juego2 (new horizontal-panel% [parent panel-juego-bajoM] ))
(define msgJugador2 (new message% [parent panel-juego-bajoM]
                          [label "Jugador2"]))


; Crea espacio de jugador3
(define panel-jugador3 (new horizontal-panel%
                           [parent panel-juego-medioR]
                           [stretchable-height #f]
                           [alignment '(center center)]
                           ))

(define panel-jugador3-juego1 (new horizontal-panel% [parent panel-juego-medioR] ))
(define panel-jugador3-juego2 (new horizontal-panel% [parent panel-juego-medioR] ))
(define msgJugador3 (new message% [parent panel-juego-medioR]
                          [label "Jugador3"]))


;; ###########################################################################

;; CREANDO LOS ESPACIOS DE CARTAS



;; CARTAS DEL JUGADOR 1

(define cartas-jugador1 '())

(define (actualiza-cartas-jugador1 nuevoValor)
  (set! cartas-jugador1 nuevoValor))


(define (encapsular-cartas-jugador1)
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta1_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta2_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta3_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta4_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta5_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta6_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta7_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta8_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta9_j1)))
  (actualiza-cartas-jugador1 (append cartas-jugador1 (list carta10_j1))))



(define carta1_j1 (new message% [parent panel-jugador1-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta2_j1 (new message% [parent panel-jugador1-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta3_j1 (new message% [parent panel-jugador1-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta4_j1 (new message% [parent panel-jugador1-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta5_j1 (new message% [parent panel-jugador1-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta6_j1 (new message% [parent panel-jugador1-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta7_j1 (new message% [parent panel-jugador1-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta8_j1 (new message% [parent panel-jugador1-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta9_j1 (new message% [parent panel-jugador1-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta10_j1 (new message% [parent panel-jugador1-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))


;; CARTAS DEL JUGADOR 2

(define cartas-jugador2 '())

(define (actualiza-cartas-jugador2 nuevoValor)
  (set! cartas-jugador2 nuevoValor))


(define (encapsular-cartas-jugador2)
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta1_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta2_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta3_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta4_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta5_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta6_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta7_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta8_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta9_j2)))
  (actualiza-cartas-jugador2 (append cartas-jugador2 (list carta10_j2))))

(define carta1_j2 (new message% [parent panel-jugador2-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta2_j2 (new message% [parent panel-jugador2-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta3_j2 (new message% [parent panel-jugador2-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta4_j2 (new message% [parent panel-jugador2-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta5_j2 (new message% [parent panel-jugador2-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta6_j2 (new message% [parent panel-jugador2-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta7_j2 (new message% [parent panel-jugador2-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta8_j2 (new message% [parent panel-jugador2-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta9_j2 (new message% [parent panel-jugador2-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta10_j2 (new message% [parent panel-jugador2-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))


;; CARTAS DEL JUGADOR 3

(define cartas-jugador3 '())

(define (actualiza-cartas-jugador3 nuevoValor)
  (set! cartas-jugador3 nuevoValor))


(define (encapsular-cartas-jugador3)
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta1_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta2_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta3_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta4_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta5_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta6_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta7_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta8_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta9_j3)))
  (actualiza-cartas-jugador3 (append cartas-jugador3 (list carta10_j3))))

(define carta1_j3 (new message% [parent panel-jugador3-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta2_j3 (new message% [parent panel-jugador3-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta3_j3 (new message% [parent panel-jugador3-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta4_j3 (new message% [parent panel-jugador3-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta5_j3 (new message% [parent panel-jugador3-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta6_j3 (new message% [parent panel-jugador3-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta7_j3 (new message% [parent panel-jugador3-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta8_j3 (new message% [parent panel-jugador3-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta9_j3 (new message% [parent panel-jugador3-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta10_j3 (new message% [parent panel-jugador3-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))

;; CARTAS DEL CRUPIER


(define cartas-crupier '())

(define (actualiza-cartas-crupier nuevoValor)
  (set! cartas-crupier nuevoValor))


(define (encapsular-cartas-crupier)
  (actualiza-cartas-crupier (append cartas-crupier (list carta1_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta2_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta3_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta4_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta5_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta6_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta7_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta8_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta9_c)))
  (actualiza-cartas-crupier (append cartas-crupier (list carta10_c))))


(define carta1_c (new message% [parent panel-crupier-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta2_c (new message% [parent panel-crupier-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta3_c (new message% [parent panel-crupier-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta4_c (new message% [parent panel-crupier-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta5_c (new message% [parent panel-crupier-juego1]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta6_c (new message% [parent panel-crupier-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta7_c (new message% [parent panel-crupier-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta8_c (new message% [parent panel-crupier-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta9_c (new message% [parent panel-crupier-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))
(define carta10_c (new message% [parent panel-crupier-juego2]
                          [label (read-bitmap "cards/vacio.png" )]))



;; Funcion actualizar imagenes

(define (llenar_cartas_jugador_aux cartasJugador espacios-cartas)
  (cond ((null? cartasJugador)
         #t)
        (else
         (send (car espacios-cartas) set-label (read-bitmap (string-append "cards/" (symbol->string (car (car cartasJugador))) (number->string (cadr (car cartasJugador))) ".png") ))
         (llenar_cartas_jugador_aux (cdr cartasJugador) (cdr espacios-cartas)))))

;(read-bitmap "cards/C1.png" )

;; (string-append "cards/" (symbol->string (car carta)) (number->string (cadr carta)) ".png")
;; (string-append "cards/" (symbol->string (car     (car cartasJugador)        )      ) (number->string (cadr (car cartasJugador))) ".png")

(define (llenar_cartas_jugador jugador cartasJugador)
  (cond ((equal? jugador "jugador1")
         (llenar_cartas_jugador_aux cartasJugador cartas-jugador1))
        ((equal? jugador "jugador2")
         (llenar_cartas_jugador_aux cartasJugador cartas-jugador2))
        ((equal? jugador "jugador3")
         (llenar_cartas_jugador_aux cartasJugador cartas-jugador3))
        ((equal? jugador "crupier")
         (llenar_cartas_jugador_aux cartasJugador cartas-crupier))))
         


(define (actualizar_cartas listaJugadores)
  (cond ((null? listaJugadores)
         (llenar_cartas_jugador "crupier" (caddr crupier)))
        (else
         (llenar_cartas_jugador (caar listaJugadores) (caddar listaJugadores))
         (actualizar_cartas (cdr listaJugadores)))))
         



(encapsular-cartas-jugador1)
(encapsular-cartas-jugador2)
(encapsular-cartas-jugador3)
(encapsular-cartas-crupier)


;======================INFORMACION================================

;Crea las etiquetas donde se mostrará la información

(define lbl-turno (new message% [parent panel-info]
                          [label "Turno de: Jugador 1"]))

(define lbl-puntos (new message% [parent panel-info]
                          [label "Puntaje actual:"]))

;Funciones para actualizar la información del turno

(define (actTurno)
  (cond ( (equal? turno 4)
          (send lbl-turno set-label "Turno de: Crupier"))
        ( else
          (send lbl-turno set-label (string-append "Turno de: Jugador " (number->string turno))))))

(define (actPuntos)
  (cond ( (equal? turno 5)
          (send lbl-puntos set-label "Juego finalizado"))
        ( (equal? turno 4)
          (send lbl-puntos set-label (string-append "Puntaje actual: " (number->string (cadr crupier)))))
        ( else
          (send lbl-puntos set-label (string-append "Puntaje actual: " (number->string (cadr (recorrer (- turno 1) listaJugadores))))))))

;-----------------------------------------------------------------


; #################### pruebas

(iniciarJuego)
;
;(actualizarListaJugadores (bCEj 3))
;(setDeck)
;(actualizarMazo (shuffle mazo))
;
;(actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
;(actualizarMazo (cdddr mazo))
;(actualizarListaJugadores (reparte-cartas listaJugadores  mazo))
;(actualizarMazo (cdddr mazo))
;
;(actualizarCrupier (dar-carta crupier  mazo))
;(actualizarMazo (cdr mazo))
;(actualizarCrupier (dar-carta crupier  mazo))
;(actualizarMazo (cdr mazo))
;
;
;listaJugadores
;crupier
;
;
;(actualizar_cartas listaJugadores)

;; (boton-pedir listaJugadores mazo)
;; (actualizar_cartas listaJugadores)
