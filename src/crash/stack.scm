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
          (substratic engine transform)
          (substratic engine components))
  (export make-stack)
  (begin

    (define layer-offset-x 2)
    (define layer-offset-y -3)

    (define (stack-renderer renderer state transform)
      (with-state state ((stack layers))
        (let loop ((layers layers)
                   (layer-index 0))
          (when (pair? layers)
            (for-each (lambda (tile)
                        (render-node renderer tile (transform-add transform
                                                                  `(,(* layer-offset-x  layer-index)
                                                                    ,(* layer-offset-y layer-index)
                                                                    0 0))))
                      (car layers))
            (loop (cdr layers) (+ layer-index 1))))))

    (define (load-stack stack-data)
      (map (lambda (layer)
             (map (lambda (tile)
                    (make-tile `((position . ((pos-x . ,(car tile))
                                              (pos-y . ,(cdr tile)))))))
                  layer))
           stack-data))

    (define (tile-run start-x pos-y num-tiles)
      (let next-tile ((tiles '())
                      (pos-x (+ start-x (- num-tiles 1)))
                      (count 1))
        (if (> count num-tiles)
            tiles
            (next-tile
              (append tiles (list (cons pos-x pos-y)))
              (- pos-x 1)
              (+ count 1)))))

    ;; This emulates the Easy board from Gnome Mahjongg
    (define test-stack
      `((;; Layer 1
         (7.5 . 0)
         (6.5 . 0)
         ,@(tile-run -5.5 -3.5 12)
         ,@(tile-run -3.5 -2.5 8)
         ,@(tile-run -4.5 -1.5 10)
         ,@(tile-run -5.5 -0.5 12)
         ,@(tile-run -5.5  0.5 12)
         ,@(tile-run -4.5  1.5 10)
         ,@(tile-run -3.5  2.5 8)
         ,@(tile-run -5.5  3.5 12)
         (-6.5 . 0))
        ( ;; Layer 2
         ,@(tile-run -2.5 -2.5 6)
         ,@(tile-run -2.5 -1.5 6)
         ,@(tile-run -2.5 -0.5 6)
         ,@(tile-run -2.5  0.5 6)
         ,@(tile-run -2.5  1.5 6)
         ,@(tile-run -2.5  2.5 6))
        ( ;; Layer 3
         ,@(tile-run -1.5 -1.5 4)
         ,@(tile-run -1.5 -0.5 4)
         ,@(tile-run -1.5  0.5 4)
         ,@(tile-run -1.5  1.5 4))
        ( ;; Layer 4
         ,@(tile-run -0.5 -0.5 2)
         ,@(tile-run -0.5  0.5 2))
        (;; Layer 5
         (0 . 0))))

    (define (stack-component)
      (make-component stack
        (layers    (load-stack test-stack))
        (renderers (add-method `(stack ,@stack-renderer)))))

    (define (make-stack component-values)
      (make-node
        'stack
        component-values: component-values
        (stack-component)))))
