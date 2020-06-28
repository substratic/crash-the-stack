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

(define-library (crash modes game)
  (import (gambit)
          (crash stack)
          (crash components editor)
          (crash controllers mouse)
          (substratic engine node)
          (substratic engine state)
          (substratic engine events)
          (substratic engine macros)
          (substratic engine keyboard)
          (substratic engine renderer)
          (substratic engine components component)
          (substratic engine components messages))
  (export game-mode)
  (begin

    (define (game-handler node context event event-sink)
      (case (event-type event)
        ((game/pause)
         (update-state node (game (> (paused #t)))))

        ((game/unpause)
         (update-state node (game (> (paused #f)))))

        ((keyboard)
         (unless (state-ref node '(game paused))
           (handle-key event
             (case-key
               ("C-r"
                 (print-message event-sink "Shuffling tiles")
                 (event-sink (make-event 'stack/shuffle)))
               ("C-t"
                 (print-message event-sink "RELOADING")
                 (reload-module "" 'crash/stack))
               ("C-M-r"
                 (print-message event-sink "Resetting board")
                 (event-sink (make-event 'stack/reset)))))))

        ((stack/changed)
         (with-state (event-data event 'stack) ((stack playable match-pairs tiles occlusion-map))
           (print-message event-sink "Pairs remaining: " (length match-pairs))
           ;; (pp match-pairs)

           ;; TODO: Check if playable is an odd number
           (if (equal? (length match-pairs) 0)
               (if (equal? (length tiles) 0)
                   (print-message event-sink "All tiles removed!")
                   (print-message event-sink "No more pairs!")))))))

    (define (game-renderer node context renderer)
      (render-clear renderer 43 4 82))

    (define (game-component #!key (paused #f))
      (make-component game
        (paused     paused)
        (state      'playing)
        (handlers   (add-method `((quit ,@quit-event-handler)
                                  (game ,@game-handler))))
        (renderers  (add-method `((game ,@game-renderer))))))

    (define (load-or-create-stack stack-file create-if-missing state)
      (if (file-exists? stack-file)
          (load-stack-file stack-file state)
          state))

    (define (game-mode #!key (stack-file #f) (editor? #f))
      (-> (make-node
            'game
            (game-component paused: editor?)
            (mouse-controller-component)  ;; Lose the rat, chief
            (stack-component)
            (messages-component)
            (when editor? (editor-component stack-file: stack-file)))
          (load-or-create-stack stack-file editor?)))))
