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
          (_hamt)
          (crash tile)
          (substratic engine node)
          (substratic engine state)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components))
  (export make-stack
          load-stack
          tile-run
          adjacent-positions)
  (begin

    (define (stack-renderer renderer state transform)
      (with-state state ((stack tiles playable occlusion-map))
        (for-each (lambda (tile)
                    (render-node renderer tile transform))
                  tiles)

        ;; Draw rects for the playable tiles
        (for-each (lambda (playable-tile)
                    (with-state playable-tile ((tile layer)
                                               (position pos-x pos-y))
                      (let ((tile-rect (tile-pos->screen-rect pos-x pos-y layer transform)))
                        (render-fill-rect
                          renderer
                          (list-set tile-rect 0 (+ (car tile-rect) 2))
                          (make-color 255 0 0 75)))))
                  playable)))


    (define (tile-exists? layer-index tile-x tile-y occlusion-map)
      (hamt-ref occlusion-map (list layer-index tile-x tile-y) #f))

    (define (tile-playable? tile occlusion-map)
      (with-state tile ((tile layer)
                        (position pos-x pos-y))
        (and
          ;; To the left and right on the same layer
          (not (and (or (tile-exists? layer (- pos-x 1) pos-y occlusion-map)
                        (tile-exists? layer (- pos-x 1) (- pos-y 0.5) occlusion-map)
                        (tile-exists? layer (- pos-x 1) (+ pos-y 0.5) occlusion-map))
                    (or (tile-exists? layer (+ pos-x 1) pos-y occlusion-map)
                        (tile-exists? layer (+ pos-x 1) (- pos-y 0.5) occlusion-map)
                        (tile-exists? layer (+ pos-x 1) (+ pos-y 0.5) occlusion-map))))
          ;; To a half-tile on the left and right on the layer above
          (not (or (tile-exists? (+ layer 1) (- pos-x 0.5) pos-y occlusion-map)
                   (tile-exists? (+ layer 1) (- pos-x 0.5) (- pos-y 0.5) occlusion-map)
                   (tile-exists? (+ layer 1) (- pos-x 0.5) (+ pos-y 0.5) occlusion-map)))
          (not (or (tile-exists? (+ layer 1) (+ pos-x 0.5) pos-y occlusion-map)
                   (tile-exists? (+ layer 1) (+ pos-x 0.5) (- pos-y 0.5) occlusion-map)
                   (tile-exists? (+ layer 1) (+ pos-x 0.5) (+ pos-y 0.5) occlusion-map))))))

    (define (get-playable-tiles tiles occlusion-map)
      (let next-tile ((tiles tiles)
                      (playable '()))
        (if (null? tiles)
            playable
            (next-tile
              (cdr tiles)
              (if (tile-playable? (car tiles) occlusion-map)
                  (append playable (list (car tiles)))
                  playable)))))

    (define (as-flonum num)
      (if (flonum? num)
          num
          (fixnum->flonum num)))

    (define (load-stack state layers)
      (let* ((occlusion-map (make-hamt))
             (tiles (let next-layer ((layers layers)
                                     (layer-index 0)
                                     (tiles '()))
                      (if (null? layers)
                          tiles
                          (next-layer
                            (cdr layers)
                            (+ layer-index 1)
                            (append tiles
                                    (map (lambda (tile)
                                           (let ((tile-x (as-flonum (car tile)))
                                                 (tile-y (as-flonum (cdr tile))))
                                            (set! occlusion-map
                                              (hamt-set occlusion-map
                                                        (list layer-index tile-x tile-y)
                                                        #t))
                                            (make-tile `((tile .     ((layer . ,layer-index)))
                                                         (position . ((pos-x . ,tile-x)
                                                                      (pos-y . ,tile-y)))))))
                                         (car layers))))))))
        (update-state state
          (stack (> (tiles tiles)
                    (playable (get-playable-tiles tiles occlusion-map))
                    (occlusion-map occlusion-map))))))

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

    (define (stack-component)
      (make-component stack
        (tiles         '())
        (playable      '())
        (occlusion-map (make-hamt))
        (renderers     (add-method `(stack ,@stack-renderer)))))

    (define (make-stack component-values)
      ;; TODO: This is a temporary hack!
      (load-stack
        (make-node
          'stack
          component-values: component-values
          (stack-component))
        test-stack))))
