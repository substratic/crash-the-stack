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

(define-library (crash game)
  (import (gambit)
          (crash stack)
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
  (export game-mode)
  (begin

    (define (game-handler event state event-sink)
      (case (event-type event)
        ((game/pause)
         (update-state state (game (> (paused #t)))))

        ((game/unpause)
         (update-state state (game (> (paused #f)))))

        ((keyboard)
         (handle-key event
           (case-key
             ("C-r"
               (println "Shuffling tiles")
               (event-sink (make-event 'stack/shuffle)))
             ("C-M-r"
               (println "Resetting board")
               (event-sink (make-event 'stack/reset))))))

        ((stack/changed)
         (println "\nStack changed!")
         (with-state (event-data event 'stack) ((stack playable match-pairs tiles occlusion-map))
           (println "Pairs remaining: " (length match-pairs))
           ;; (pp match-pairs)

           ;; TODO: Check if playable is an odd number
           (if (equal? (length match-pairs) 0)
               (if (equal? (length tiles) 0)
                   (println "All tiles removed!")
                   (println "No more pairs!")))))))

    (define (game-renderer renderer state transform)
      #!void)

    (define (game-component)
      (make-component game
        (paused     #f)
        (state      'playing)
        (handlers   (add-method `((quit ,@quit-event-handler)
                                  (game ,@game-handler))))
        (renderers  (add-method `((game ,@game-renderer))))))

    ;; A simple test stack that I can finish faster
    (define small-stack
      `((;; Layer 1
         ,@(tile-run -2.5 -1.5 5)
         ,@(tile-run -2.5 -0.5 5)
         ,@(tile-run -2.5  0.5 5)
         ,@(tile-run -2.5  1.5 5))
        (;; Layer 2
         ,@(tile-run -2.0 -1.0 4)
         ,@(tile-run -2.0 -0.0 4)
         ,@(tile-run -2.0  1.0 4))
        (;; Layer 3
         ,@(tile-run -1.0 -0.5 2)
         ,@(tile-run -1.0  0.5 2))))

    ;; This emulates the Easy board from Gnome Mahjongg
    (define gnome-mahjongg-easy
      `((;; Layer 1
         ,@(tile-run -5.5 -3.5 12)
         ,@(tile-run -3.5 -2.5 8)
         ,@(tile-run -4.5 -1.5 10)
         (7.5 . 0)
         (6.5 . 0)
         ,@(tile-run -5.5 -0.5 12)
         ,@(tile-run -5.5  0.5 12)
         ,@(tile-run -4.5  1.5 10)
         ,@(tile-run -3.5  2.5 8)
         ,@(tile-run -5.5  3.5 12)
         (-6.5 . 0))
        (;; Layer 2
         ,@(tile-run -2.5 -2.5 6)
         ,@(tile-run -2.5 -1.5 6)
         ,@(tile-run -2.5 -0.5 6)
         ,@(tile-run -2.5  0.5 6)
         ,@(tile-run -2.5  1.5 6)
         ,@(tile-run -2.5  2.5 6))
        (;; Layer 3
         ,@(tile-run -1.5 -1.5 4)
         ,@(tile-run -1.5 -0.5 4)
         ,@(tile-run -1.5  0.5 4)
         ,@(tile-run -1.5  1.5 4))
        (;; Layer 4
         ,@(tile-run -0.5 -0.5 2)
         ,@(tile-run -0.5  0.5 2))
        (;; Layer 5
         (0 . 0))))

    (define (game-mode #!key (initial-area #f))
      ;; TODO: This is a temporary hack!
      (load-stack
        (make-node
          'game
          (game-component)
          (mouse-controller-component)  ;; Lose the rat, chief
          (stack-component)
          (messages-component))
        small-stack))))
