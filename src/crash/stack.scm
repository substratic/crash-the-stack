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

(define-library (crash stack)
  (import (gambit)
          (crash tile)
          (substratic engine node)
          (substratic engine state)
          (substratic engine renderer)
          (substratic engine components))
  (export make-stack)
  (begin

    (define (stack-renderer renderer state transform)
      (with-state state ((stack layers))
        (let ((offset-x 0)
              (offset-y 0))
          ;; TODO: Use a looping let
          ;; (let loop ((offset-x 0)
          ;;            (offset-y 0)
          ;;            (remaining-layers layers)))
          (for-each (lambda (layer)
                      (for-each (lambda (tile)
                                  ;; TODO: Use transform for offset?
                                  (render-node renderer tile transform))
                                layer)
                      (set! offset-x (+ offset-x (/ 2 tile-width)))
                      (set! offset-y (+ offset-y (truncate (floor (/ 4 tile-height))))))
                    layers))))

    (define (stack-component)
      (make-component stack
        (layers    `((;; Layer 1
                      ,(make-tile '((position . ((pos-x . 100) (pos-y . 100))))))
                     (;; Layer 2
                      ,(make-tile '((position . ((pos-x . 100) (pos-y . 100))))))))
        (renderers (add-method `(stack ,@stack-renderer)))))

    (define (make-stack component-values)
      (make-node
        'stack
        component-values: component-values
        (stack-component)))))
