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

(define-library (crash components menu)
  (import (gambit)
          (substratic sdl2)
          (substratic engine node)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine events)
          (substratic engine keyboard)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components))
  (export menu-component)
  (begin

    (define (move-selection delta menu-items)
      (lambda (selection)
        (modulo (+ selection delta)
                (length menu-items))))

    (define (menu-handler event state event-sink)
      (define (execute-selection selection menu-items)
        (let ((executor (caddr (list-ref menu-items selection))))
          (if executor
              (executor state event-sink)
              state)))

      (with-state state ((menu selection menu-items))
        (case (event-type event)
          ((keyboard)
           (handle-key event
             (case-key
               ("j"   (update-state state (menu (> (selection (move-selection +1 menu-items))))))
               ("k"   (update-state state (menu (> (selection (move-selection -1 menu-items))))))
               ("RET" (execute-selection selection menu-items))
               ("SPC" (execute-selection selection menu-items))))))))

    (define (menu-updater state time-step event-sink)
      state)

    (define (menu-renderer renderer state transform)
      (with-state state ((menu selection menu-items))
        (let ((index  0)
              (screen-center-x (/ (transform-width transform) 2))
              (start-y (/ (transform-height transform) 2)))
          (for-each (lambda (item)
                      (render-text renderer
                                   (cadr item)
                                   *default-font*
                                   screen-center-x
                                   (+ (* index 20) start-y)
                                   color: (if (equal? index selection)
                                              '(231 227 0 0)
                                              '(255 255 255 0))
                                   align: 'center)
                      (set! index (+ index 1)))
                    menu-items))))

    (define (menu-component #!key (items '()))
      (make-component menu
        (selection  0)
        (menu-items items)
        (updaters   (add-method `(menu ,@menu-updater)))
        (handlers   (add-method `(menu ,@menu-handler)))
        (renderers  (add-method `(menu ,@menu-renderer)))))))
