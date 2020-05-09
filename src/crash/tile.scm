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

(define-library (crash tile)
  (import (gambit)
          (substratic sdl2)
          (substratic engine node)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components))
  (export make-tile
          tile-width
          tile-height
          load-tile-assets)
  (begin

    (define tile-width 30)
    (define tile-height 36)
    (define tile-image #f)

    (define (load-tile-assets)
      (set! tile-image (load-asset "images/tile.png")))

    (define (tile-pos->screen-rect tile-x tile-y transform)
      (let ((screen-x (+ (- (* tile-x tile-width)  (/ tile-width 2))
                         (/ (transform-width transform) 2)
                         (transform-x transform)))
            (screen-y (+ (- (* tile-y tile-height) (/ tile-height 2))
                         (/ (transform-height transform) 2)
                         (transform-y transform))))
        (list (exact (truncate screen-x))
              (exact (truncate screen-y))
              tile-width tile-height)))

    (define (tile-renderer renderer state transform)
      (with-state state ((position pos-x pos-y)
                         (tile symbol color))
        (let* ((tile-rect (tile-pos->screen-rect pos-x pos-y transform))
               (tile-x (car  tile-rect))
               (tile-y (cadr tile-rect)))
          (render-image renderer
                        tile-image
                        tile-x
                        tile-y))))

    (define (tile-component)
      (make-component tile
        (symbol    "X")
        (color     (make-color 0 255 0))
        (renderers (add-method `(tile ,@tile-renderer)))))

    (define (make-tile component-values)
      (make-node
        'tile
        component-values: component-values
        (tile-component)
        (position-component)))))
