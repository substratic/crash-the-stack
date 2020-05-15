;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;;
;; This file is part of Crash The Stack.
;; https://github.com/substratic/crash-the-stack
;;
;; Crash The Stack is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Crash The Stack is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Crash The Stack.  If not, see <https://www.gnu.org/licenses/>.

(define-library (crash modes title-screen)
  (import (gambit)
          (crash modes game)
          (crash controllers mouse)
          (substratic sdl2)
          (substratic engine node)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine events)
          (substratic engine keyboard)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components))
  (export title-screen-mode)
  (begin

    (define (ease-in-out value)
      (cond
        ((> value 0.75) (max 0.0 (- 1.0 (/ (- value 0.75) 0.25))))
        ((> value 0.25) 1.0)
        (else (min 1.0 (/ value 0.25)))))

    (define *title-phase-time* 4.0)

    (define (title-cards-updater state time-step event-sink)
      (with-state state ((title-cards phase sequence-time))
        (let* ((phase-time *title-phase-time*)
               (sequence-time (+ time-step sequence-time)))

          (when (> sequence-time phase-time)
            (set! phase (+ phase 1))
            (set! sequence-time (- sequence-time phase-time)))

          (when (equal? phase 4)
            (event-sink (make-event 'engine/change-mode data: `((next-mode ,@game-mode)))))

          (update-state state
            (title-cards (> (phase phase)
                            (sequence-time sequence-time)))))))

    (define (title-cards-renderer renderer state transform)
      (render-clear renderer 0 0 0)

      (with-state state ((title-cards phase sequence-time))
        (let* ((width (transform-width transform))
               (height (transform-height transform))
               (fade-alpha (ease-in-out (/ sequence-time *title-phase-time*)))
               (text (case phase
                      ((1) "Flux Harmonic Presents")
                      ((2) "A game by David Wilson")
                      ((3) "CRASH THE STACK")
                      (else " "))))

          (render-text renderer text *default-font*
                       (/ width 2) (/ height 2)
                       align: 'center
                       alpha: (exact (floor (* fade-alpha 255)))))))

    (define (title-cards-component)
      (make-component title-cards
        (phase          1)
        (sequence-time  0.0)
        (handlers       (add-method `(quit ,@quit-event-handler)))
        (updaters       (add-method `(sequence ,@title-cards-updater)))
        (renderers      (add-method `(title-screen ,@title-cards-renderer)))))

    (define (title-screen-mode)
      (make-node
        'title-screen
        (title-cards-component)))))
