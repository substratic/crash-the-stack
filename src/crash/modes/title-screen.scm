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
          (crash components menu)
          (crash controllers mouse)
          (substratic sdl2)
          (substratic engine node)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine easing)
          (substratic engine events)
          (substratic engine macros)
          (substratic engine logging)
          (substratic engine keyboard)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components))
  (export title-screen-mode)
  (begin

    (define title-phase-time 4.0)

    (define (timed-phase #!key duration on-start on-end)
      (define (timed-phase-updater node context time-step event-sink)
        (with-state node ((timed-phase phase-time))
          (set! phase-time (+ phase-time time-step))
          (if (> phase-time duration)
              (-> node
                  (remove-component 'timed-phase)
                  (on-end))
              (update-state node (timed-phase (> (phase-time phase-time)))))))

      (lambda (node)
        (-> node
            ((make-component timed-phase
               (phase-time 0.0)
               (updaters   (add-method `(timed-phase ,@timed-phase-updater)))))
            (on-start))))

    (define (publisher-card-renderer node context renderer)
      (render-clear renderer 0 0 0)
      (render-text renderer
                   "Flux Harmonic Presents"
                   *default-font*
                   (/ (state-ref context 'screen-width) 2)
                   (/ (state-ref context 'screen-height) 2)
                   align: 'center))

    (define (developer-card-renderer node context renderer)
      (render-clear renderer 0 0 0)
      (render-text renderer
                   "A game by David Wilson"
                   *default-font*
                   (/ (state-ref context 'screen-width) 2)
                   (/ (state-ref context 'screen-height) 2)
                   align: 'center))

    (define (title-screen-renderer node context renderer)
      (define (render-line text screen-y #!key (font *default-font*)
                                               (color #f))
        (render-text renderer
                     text
                     font
                     (/ (state-ref context 'screen-width) 2)
                     screen-y
                     color: color
                     align: 'center))

      (render-clear renderer 0 0 0)
      (render-line "CRASH THE STACK"
                   (/ (state-ref context 'screen-height) 4))

      ;; Menu is rendered by menu-component

      (render-line "//  Copyright (c) 2020 David Wilson  ||  Published by Flux Harmonic LLC  \\\\"
                    (- (state-ref context 'screen-height) 25)
                    font: *default-font-small*))

    (define (setup-game-mode)
      ;; TODO: Support continuing from saved game
      (game-mode stack-file: "stacks/test/gnome-easy.scm"))

    (define (start-game)
      (lambda (state event-sink)
        (event-sink (make-event
                      'engine/change-mode
                      data: `((next-mode ,@setup-game-mode))))))

    (define main-menu-items
      `((new      "New Game" ,(start-game))

        (continue "Continue" ,(start-game))
        (options  "Options"  #f)
        (credits  "Credits"  #f)
        (exit     "Exit"     ,(lambda (state event-sink)
                                (event-sink (make-event 'engine/quit))))))

    (define publisher-card-phase
      (timed-phase
        duration: title-phase-time
        on-start:
        (lambda (node)
          (-> node
              ((make-component card
                 (renderers (add-method `(card ,@publisher-card-renderer)))))
              ((screen-fade-component
                duration: title-phase-time
                mode: 'in-out))))

        on-end:
        (lambda (node)
          (developer-card-phase node))))

    (define developer-card-phase
      (timed-phase
        duration: title-phase-time
        on-start:
        (lambda (node)
          (-> node
              ((make-component card
                 (renderers (add-method `(card ,@developer-card-renderer)))))
              ((screen-fade-component
                duration: title-phase-time
                mode: 'in-out))))

        on-end:
        (lambda (node)
          (-> node
              (remove-component 'card)
              (remove-component 'screen-fade)
              (title-screen-phase)))))

    (define (title-screen-phase node)
      (-> node
          ((make-component title-screen
             (renderers (add-method `(title-screen ,@title-screen-renderer)))))
          ((menu-component items: main-menu-items))))

    (define (title-screen-mode)
      (-> (make-node 'title-screen)
          (publisher-card-phase)))))
