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
          (substratic engine renderer)
          (substratic engine components))
  (export make-tile
          tile-width
          tile-height)
  (begin

    (define tile-width 16)
    (define tile-height 24)

    (define (tile-renderer renderer state transform)
      (with-state state ((position pos-x pos-y)
                         (tile symbol color))
        (render-rect renderer (list pos-x pos-y tile-width tile-height) color)))

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
