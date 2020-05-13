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
          (substratic engine node)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine events)
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
         (update-state state (game (> (paused #f)))))))

    (define (game-component)
      (make-component game
        (paused     #f)
        (handlers   (add-method `((quit ,@quit-event-handler)
                                  (game ,@game-handler))))))

    ;; This emulates the Easy board from Gnome Mahjongg
    (define test-stack
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
        test-stack))))
