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
          (substratic engine events)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components))
  (export make-stack
          stack-component
          load-stack
          tile-run)
  (begin

    (define board-width 16)
    (define board-height 9)
    (define board-screen-width #f)
    (define board-screen-height #f)
    (define board-start-x #f)
    (define board-start-y #f)

    (define screen-width #f)
    (define screen-height #f)

    (define show-playable-tiles #f)

    (define (playable-tile-at-point pos-x pos-y playable-tiles)
      ;; NOTE: This is a naive approach but seemingly more expedient
      ;; than a calculation-based lookup approach
      (let next-playable ((tiles playable-tiles))
        (if (null? tiles)
            #f
            (let ((screen-rect
                    (tile-pos->screen-rect (car tiles) screen-width screen-height)))
              (if (and (>= pos-x (car screen-rect))
                       (<  pos-x (+ (car screen-rect)
                                    (+ (caddr screen-rect) layer-offset-x)))
                       (>= pos-y (cadr screen-rect))
                       (<  pos-y (+ (cadr screen-rect) (cadddr screen-rect))))
                  (car tiles)
                  (next-playable (cdr tiles)))))))

    (define (stack-handler event state event-sink)
      (case (event-type event)
       ((stack/select-tile)
        (println "Selecting tile at: " (event-data event 'tile-x) " " (event-data event 'tile-y)))
       ((stack/select-at)
        (when screen-width
          (with-state state ((stack layer-count playable occlusion-map))
            (let ((selected-tile
                    (playable-tile-at-point
                      (event-data event 'pos-x)
                      (event-data event 'pos-y)
                      (state-ref state '(stack playable)))))
              (when selected-tile
                (update-state state (stack (> (selected-tile selected-tile)))))))))))

    (define (screen-pos->tile-pos screen-x screen-y screen-width screen-height layer-index)
      (let* ((board-x (- screen-x board-start-x (* (+ layer-index 1) layer-offset-x)))
             (board-y (- screen-y board-start-y (* layer-index layer-offset-y)))
             (tile-x (inexact (/ (- (truncate (/ board-x (/ tile-width  2))) board-width) 2)))
             (tile-y (inexact (/ (- (truncate (/ board-y (/ tile-height 2))) board-height) 2))))
        (list layer-index tile-x tile-y)))

    (define (stack-renderer renderer state transform)
      ;; Store the screen and board sizes
      (unless screen-width
        (set! screen-width  (transform-width  transform))
        (set! screen-height (transform-height transform))
        (set! board-screen-width  (* board-width  tile-width))
        (set! board-screen-height (* board-height tile-height))
        (set! board-start-x (/ (- screen-width  (* board-width  tile-width)) 2))
        (set! board-start-y (/ (- screen-height (* board-height tile-height)) 2)))

      (with-state state ((stack tiles playable selected-tile
                                occlusion-map select-region))
        (for-each (lambda (tile)
                    (render-node renderer tile transform))
                  tiles)

        (when show-playable-tiles
          Draw rects for the playable tiles
          (for-each (lambda (playable-tile)
                      (let ((tile-rect (tile-pos->screen-rect playable-tile
                                                              screen-width
                                                              screen-height)))
                        (render-fill-rect
                          renderer
                          (list-set tile-rect 0 (+ (car tile-rect) 2))
                          (make-color 255 0 0 75))))
                    playable))

        (when selected-tile
          (let ((tile-rect (tile-pos->screen-rect selected-tile
                                                  screen-width
                                                  screen-height)))
            ;; TODO: Animate selection color
            (render-rect
              renderer
              (list (+ (car tile-rect)    layer-offset-x 3)
                    (+ (cadr tile-rect)   3)
                    (- (caddr tile-rect)  5)
                    (- (cadddr tile-rect) 5))
              (make-color 25 255 75 150))))))

    ;; Randomize the random source for "real" randomness
    (random-source-randomize! default-random-source)

    (define (generate-glyphs tile-count)
      (let next-pair ((count (/ tile-count 2))
                      (glyphs '()))
        (if (< count 0)
            (list-sort
              (lambda (a b)
                (equal? 1 (random-integer 2)))
              glyphs)
            (next-pair
              (- count 1)
              (let ((glyph (random-integer 42))) ;; 42 possible tile types
                (append glyphs (list glyph glyph)))))))

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

    (define (get-tile-pos tile)
      (with-state tile ((tile layer)
                        (position pos-x pos-y))
        (list layer pos-x pos-y)))

    (define (get-playable-tiles tiles occlusion-map)
      (let next-tile ((tiles tiles)
                      (playable '()))
        (if (null? tiles)
            playable
            (next-tile
              (cdr tiles)
              (if (tile-playable? (car tiles) occlusion-map)
                  (append playable (list (get-tile-pos (car tiles))))
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
                                         (car layers)))))))
             (tiles (map (lambda (tile glyph)
                           (update-state tile (tile (> (glyph (number->string glyph))))))
                         tiles
                         (generate-glyphs (length tiles)))))

        (update-state state
          (stack (> (tiles tiles)
                    (layer-count (length layers))
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

    (define (stack-component)
      (make-component stack
        (tiles         '())
        (layer-count   0)
        (playable      '())
        (selected-tile #f)
        (occlusion-map (make-hamt))
        (handlers      (add-method `(stack ,@stack-handler)))
        (renderers     (add-method `(stack ,@stack-renderer)))))))
