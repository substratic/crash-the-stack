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

    (define (game-renderer renderer game-state transform)
      (let* ((screen-width  (transform-width  transform))
             (screen-height (transform-height transform)))
        (render-node renderer (state-ref game-state '(game stack)) transform)
        #!void))

    (define (game-component)
      (make-component game
        (paused     #f)
        (stack      (make-stack '()))
        (handlers   (add-method `((quit ,@quit-event-handler)
                                  (game ,@game-handler))))
        (renderers  (add-method `(game ,@game-renderer)))))

    (define (game-mode #!key (initial-area #f))
      (make-node
        'game
        (game-component)
        (messages-component)))))
