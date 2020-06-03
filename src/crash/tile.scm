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
          layer-offset-x
          layer-offset-y
          load-tile-assets
          tile-pos->screen-rect)
  (begin

    (define tile-width 30)
    (define tile-height 36)
    (define tile-image #f)

    (define layer-offset-x 2)
    (define layer-offset-y -3)

    (define (load-tile-assets)
      (set! tile-image (load-asset "images/tile.png")))

    (define (tile-pos->screen-rect tile-pos screen-width screen-height)
      (let* ((layer-index (car tile-pos))
             (tile-x (cadr tile-pos))
             (tile-y (caddr tile-pos))
             (screen-x (+ (- (* tile-x tile-width)  (/ tile-width 2))
                          (/ screen-width 2)
                          (* layer-offset-x layer-index)))
             (screen-y (+ (- (* tile-y tile-height) (/ tile-height 2))
                          (/ screen-height 2)
                          (* layer-offset-y layer-index))))
        (list (exact (truncate screen-x))
              (exact (truncate screen-y))
              tile-width tile-height)))

    (define glyph-color #f)

    (define (tile-renderer node context renderer)
      (with-state node ((position pos-x pos-y)
                        (tile glyph layer))
        (let* ((tile-rect (tile-pos->screen-rect (list layer pos-x pos-y)
                                                 (state-ref context 'screen-width)
                                                 (state-ref context 'screen-height)))
               (tile-x (car  tile-rect))
               (tile-y (cadr tile-rect))
               (glyph-x (+ (+ layer-offset-x 2) tile-x (/ tile-width 2)))
               (glyph-y (+ tile-y (/ tile-height 2) 3))) ;; TODO: No magic constants

          ;; Draw the tile
          (render-image renderer
                        tile-image
                        tile-x
                        tile-y)

          (when (not glyph-color)
            (set! glyph-color (make-color 52 158 255)))

          ;; Glyph text
          (render-text
            renderer
            glyph
            *default-font*
            glyph-x
            glyph-y
            align: 'center
            color: glyph-color))))

    (define (tile-component)
      (make-component tile
        (glyph     "#")
        (layer     0)
        (renderers (add-method `(tile ,@tile-renderer)))))

    (define (make-tile component-values)
      (make-node
        'tile
        component-values: component-values
        (tile-component)
        (position-component)))))
