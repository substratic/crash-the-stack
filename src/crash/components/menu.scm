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
          (substratic engine node)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine events)
          (substratic engine keyboard)
          (substratic engine renderer)
          (substratic engine components component))
  (export menu-component)
  (begin

    (define (move-selection delta menu-items)
      (lambda (selection)
        (modulo (+ selection delta)
                (length menu-items))))

    (define (menu-handler node context event event-sink)
      (define (execute-selection selection menu-items)
        (let ((executor (caddr (list-ref menu-items selection))))
          (if executor
              (executor node event-sink)
              node)))

      (with-state node ((menu selection menu-items))
        (case (event-type event)
          ((keyboard)
           (handle-key event
             (case-key
               ("j"    (update-state node (menu (> (selection (move-selection +1 menu-items))))))
               ("k"    (update-state node (menu (> (selection (move-selection -1 menu-items))))))
               ("up"   (update-state node (menu (> (selection (move-selection -1 menu-items))))))
               ("down" (update-state node (menu (> (selection (move-selection +1 menu-items))))))
               ("RET"  (execute-selection selection menu-items))
               ("SPC"  (execute-selection selection menu-items))))))))

    (define (menu-updater node context time-step event-sink)
      node)

    (define (menu-renderer node context renderer)
      (with-state node ((menu selection menu-items))
        (let ((index  0)
              (screen-center-x (/ (state-ref context 'screen-width) 2))
              (start-y (/ (state-ref context 'screen-height) 2)))
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
