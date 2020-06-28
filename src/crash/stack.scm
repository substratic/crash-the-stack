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
          (substratic engine assets)
          (substratic engine events)
          (substratic engine renderer)
          (substratic engine components component)
          (substratic engine components messages))
  (export make-stack
          stack-component
          load-stack
          load-stack-file
          get-playable-tiles
          tile-placeable?
          make-occlusion-map)
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

    (define (stack-handler node context event event-sink)
      (or (case (event-type event)
           ((stack/shuffle)
            (init-stack (state-ref node '(stack tiles)) node))

           ((stack/store-initial)
            (update-state node (stack
                                (> (initial-stack (get-stack-data node))))))

           ((stack/reset)
            (load-stack (state-ref node '(stack initial-stack)) node))

           (else #f))

          (if (state-ref node '(game paused))
              (case (event-type event)
                ((stack/insert-or-remove-tile)
                 (insert-or-remove-tile node (event-data event 'tile-pos)))

                ((stack/save)
                 (let ((stack-file (event-data event 'stack-file)))
                   (print-message event-sink "Saving stack to file: " stack-file)
                   (save-stack node stack-file)))

                (else #f))

              (case (event-type event)
               ((stack/select-tile)
                (print-message event-sink "Selecting tile at: " (event-data event 'tile-x) " " (event-data event 'tile-y)))

               ((stack/shuffle)
                (init-stack (state-ref node '(stack tiles)) node))

               ((stack/select-at)
                (with-state node ((stack layer-count playable tiles
                                         selected-tile occlusion-map))
                  (let ((new-selected-tile (playable-tile-at-point
                                             (event-data event 'pos-x)
                                             (event-data event 'pos-y)
                                             (state-ref node '(stack playable)))))
                    (when new-selected-tile
                      (print-message event-sink "Selected tile at pos:" new-selected-tile)
                      (if (and selected-tile
                               (not (equal? selected-tile new-selected-tile))
                               (tiles-match? selected-tile new-selected-tile tiles))
                          (let ((new-node (remove-match node selected-tile new-selected-tile)))
                            (event-sink
                              (make-event 'stack/changed
                                           data: `((stack ,@new-node))))
                            new-node)
                          (update-state node (stack (> (selected-tile new-selected-tile)))))))))

               (else #f)))

          ;; If we reached this case, no events were handled
          #!void))

    (define (screen-pos->tile-pos screen-x screen-y screen-width screen-height layer-index)
      (let* ((board-x (- screen-x board-start-x (* (+ layer-index 1) layer-offset-x)))
             (board-y (- screen-y board-start-y (* layer-index layer-offset-y)))
             (tile-x (inexact (/ (- (truncate (/ board-x (/ tile-width  2))) board-width) 2)))
             (tile-y (inexact (/ (- (truncate (/ board-y (/ tile-height 2))) board-height) 2))))
        (list layer-index tile-x tile-y)))

    (define (stack-renderer node context renderer)
      ;; Store the screen and board sizes
      (unless screen-width
        (set! screen-width  (state-ref context 'screen-width))
        (set! screen-height (state-ref context 'screen-height))
        (set! board-screen-width  (* board-width  tile-width))
        (set! board-screen-height (* board-height tile-height))
        (set! board-start-x (/ (- screen-width  (* board-width  tile-width)) 2))
        (set! board-start-y (/ (- screen-height (* board-height tile-height)) 2)))

      (with-state node ((stack tiles playable selected-tile layer-opacity
                               occlusion-map select-region show-glyphs?))
        (let ((context (update-state context (show-glyphs? (not (state-ref node '(game paused))))
                                             (layer-opacity layer-opacity))))
          (for-each (lambda (tile)
                      (render-node tile context renderer))
                    tiles))

        (when show-playable-tiles
          ;; Draw rects for the playable tiles
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
              (list (+ (car tile-rect)    2)
                    (+ (cadr tile-rect)   3)
                    (- (caddr tile-rect)  5)
                    (- (cadddr tile-rect) 5))
              (make-color 25 255 75 150))))))

        ;; TODO: Draw reticules on the top and side to line up with selection for layer
        ;; Make it look vaguely techy!
        ;; Highlight tiles to show possible selections rather than drawing an ugly rectangle

    ;; -- Stack initialization -------------------------------------------------

    ;; Randomize the random source for "real" randomness
    (random-source-randomize! default-random-source)

    (define (generate-glyphs tile-count)
      (let next-pair ((count (/ tile-count 2))
                      (glyphs '()))
        (if (equal? count 0)
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
      (define (occluded? layer pos-x pos-y)
        (or (tile-exists? layer pos-x pos-y occlusion-map)
            (tile-exists? layer pos-x (- pos-y 0.5) occlusion-map)
            (tile-exists? layer pos-x (+ pos-y 0.5) occlusion-map)))

      (with-state tile ((tile layer)
                        (position pos-x pos-y))
        (and
          ;; To the left and right on the same layer
          (not (and (occluded? layer (- pos-x 1) pos-y)
                    (occluded? layer (+ pos-x 1) pos-y)))

          ;; At the same position on the layer above
          (not (occluded? (+ layer 1) pos-x pos-y))

          ;; To a half-tile on the left and right on the layer above
          (not (occluded? (+ layer 1) (- pos-x 0.5) pos-y))
          (not (occluded? (+ layer 1) (+ pos-x 0.5) pos-y)))))

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

    (define (get-match-pairs playable tiles)
      (let ((glyph-table (make-hamt))
            (matches '()))
        (for-each (lambda (tile-pos)
                    (let* ((tile (find (tile-at-pos? tile-pos) tiles))
                           (glyph (state-ref tile '(tile glyph)))
                           (match-pos (hamt-ref glyph-table glyph #f)))
                      (if match-pos
                          (begin
                            (set! matches (append matches (list (cons match-pos tile-pos))))
                            (set! glyph-table (hamt-set glyph-table glyph #f)))
                          (set! glyph-table (hamt-set glyph-table glyph tile-pos)))))
                  playable)
        matches))

    (define (as-flonum num)
      (let ((new-num
              (if (flonum? num)
                  num
                  (fixnum->flonum num))))
        ;; This tweak is surpisingly necessary in case a board author
        ;; mechanically edits a coordinate to contain -0.0.  Gambit will
        ;; "helpfully" retain the sign on the number even though it
        ;; doesn't make any real sense.  Since we use these numbers
        ;; verbatim to look up occluding tiles, the sign will cause
        ;; positions arrived at by addition to not match what was stored
        ;; in the hash map.
        (if (equal? new-num -0.) 0. new-num)))

    (define (make-occlusion-map tiles)
      (fold (lambda (tile occlusion-map)
              (hamt-set occlusion-map (get-tile-pos tile) #t))
            (make-hamt)
            tiles))

    (define (init-stack tiles state)
      (let* ((tiles (map (lambda (tile glyph)
                           (update-state tile (tile (> (glyph (number->string glyph))))))
                         tiles
                         (generate-glyphs (length tiles))))
             (occlusion-map (make-occlusion-map tiles))
             (playable (get-playable-tiles tiles occlusion-map)))

        (update-state state
          (stack (> (tiles tiles)
                    (tile-count (length tiles))
                    (playable    playable)
                    (match-pairs (get-match-pairs playable tiles))
                    (occlusion-map occlusion-map))))))

    ;; -- Stack loading --------------------------------------------------------

    (define (load-stack-file stack-file state)
      (let* ((stack-data (read (open-file stack-file))))
        (when (not (equal? (car stack-data) 'stack))
          (raise (string-append "load-stack: Cannot load data of type " (car area))))
        (load-stack (cdr stack-data) state)))

    (define (stack-data->layers stack-data)
          (map (lambda (layer)
                (fold (lambda (pos-or-run pos-list)
                        (if (list? pos-or-run)
                            (if (equal? (car pos-or-run) 'run)
                                (append pos-list (apply tile-run (cdr pos-or-run)))
                                (raise (string-append "load-stack-file: Unexpected operation " (car pos-or-run))))
                            (append pos-list (list pos-or-run))))
                      '()
                      layer))
              stack-data))

    (define (load-stack stack-data state)
      (let* ((layers (stack-data->layers stack-data))
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
                                            (make-tile `((tile .     ((layer . ,layer-index)))
                                                         (position . ((pos-x . ,tile-x)
                                                                      (pos-y . ,tile-y)))))))
                                         (car layers))))))))
           (update-state (init-stack tiles state)
             (stack (> (initial-stack layers))))))

    (define (tile-sorter new-tile old-tile)
      ;; The question this function answers is "which tile gets rendered first?"
      ;; Earlier tiles in the list are rendered first.
      ;; - Tiles in lower layers are rendered before higher layers
      ;; - Tiles on the same layer are rendered top to bottom, right to left
      ;; 1000 is an arbitrary constant used to ensure higher layers are
      ;; always sorted after lower layers.
      (< (with-state new-tile ((position pos-x pos-y) (tile layer))
           (+ (* layer 1000) (* pos-x -1.0) pos-y))
         (with-state old-tile ((position pos-x pos-y) (tile layer))
           (+ (* layer 1000) (* pos-x -1.0) pos-y))))

    ;; -- Stack operations -----------------------------------------------------

    (define (find find-proc items)
      (let next-item ((items items))
        (if (pair? items)
            (if (find-proc (car items))
                (car items)
                (next-item (cdr items)))
            #f)))

    (define (tile-at-pos? tile-pos)
      (lambda (tile)
        (equal? (get-tile-pos tile) tile-pos)))

    (define (tiles-match? selected-tile-pos match-tile-pos tiles)
      (let ((selected-tile (find (tile-at-pos? selected-tile-pos) tiles))
            (match-tile    (find (tile-at-pos? match-tile-pos) tiles)))
        (equal? (state-ref selected-tile '(tile glyph))
                (state-ref match-tile '(tile glyph)))))

    (define (remove-match state selected-tile-pos match-tile-pos)
      (with-state state ((stack tiles playable occlusion-map))
        ;; Remove tiles from tiles
        (set! tiles (remp (tile-at-pos? selected-tile-pos) tiles))
        (set! tiles (remp (tile-at-pos? match-tile-pos) tiles))

        ;; Clear spots in occlusion-map
        (set! occlusion-map (hamt-set occlusion-map selected-tile-pos #f))
        (set! occlusion-map (hamt-set occlusion-map match-tile-pos #f))

        ;; Recalculate playable tiles
        (set! playable (get-playable-tiles tiles occlusion-map))

        (update-state state (stack (> (tiles tiles)
                                      (playable playable)
                                      (match-pairs (get-match-pairs playable tiles))
                                      (selected-tile #f)
                                      (occlusion-map occlusion-map))))))


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


    ;; -- Stack saving ---------------------------------------------------------

    (define (get-stack-data node)
      (let next-tile ((tiles (state-ref node '(stack tiles)))
                      (current-layer 0)
                      (layer-tiles '())
                      (layers '()))
        (if (not (pair? tiles))
            (append layers (list layer-tiles))
            ;; Decide whether we're still working on the current layer
            ;; or should switch to the next layer
            (with-state (car tiles) ((tile layer)
                                     (position pos-x pos-y))
              (let* ((new-layer? (> layer current-layer))
                     (current-layer (if new-layer? (+ current-layer 1) current-layer))
                     (tile (cons pos-x pos-y))
                     (layers (if new-layer?
                                 (append layers (list layer-tiles))
                                 layers))
                     (layer-tiles (if new-layer?
                                      (list tile)
                                      (append layer-tiles (list tile)))))
                (next-tile (cdr tiles)
                           current-layer
                           layer-tiles
                           layers))))))

    (define (save-stack node stack-file)
      (with-output-to-file stack-file
        (lambda ()
          (pretty-print (cons 'stack (get-stack-data node))))))

    ;; -- Utilities for stack editing ------------------------------------------

    (define (insert-tile node tile-pos new-tile)
      (update-state
       node
       (stack
        (> (tiles (lambda (tiles)
                    (insert-sorted new-tile tile-sorter tiles)))
           (tile-count (lambda (tile-count)
                         (+ tile-count 1)))
           (occlusion-map (lambda (occlusion-map)
                            (hamt-set occlusion-map tile-pos #t)))))))

    (define (remove-tile node tile-pos)
      (update-state
       node
       (stack
        (> (tiles (lambda (tiles)
                    (remp (tile-at-pos? tile-pos) tiles)))
           (tile-count (lambda (tile-count)
                         (- tile-count 1)))
           (occlusion-map (lambda (occlusion-map)
                            (hamt-set occlusion-map tile-pos #f)))))))

    ;; This procedure is invoked by the editor to set or clear a tile
    ;; at the given tile position.
    (define (insert-or-remove-tile node tile-pos)
      (let* ((layer (tile-pos-layer tile-pos))
             (pos-x (tile-pos-x tile-pos))
             (pos-y (tile-pos-y tile-pos))
             (exists? (tile-exists? layer pos-x pos-y (state-ref node '(stack occlusion-map)))))
        (if exists?
            (remove-tile node tile-pos)
            (insert-tile
             node
             tile-pos
             (make-tile `((tile .     ((layer . ,layer)))
                          (position . ((pos-x . ,pos-x)
                                       (pos-y . ,pos-y)))))))))

    ;; Determines whether a tile can be placed at the given tile-pos
    (define (tile-placeable? node tile-pos)
      ;; A tile is placeable when there isn't a tile at any of its corner positions
      (let ((layer (tile-pos-layer tile-pos))
            (pos-x (tile-pos-x tile-pos))
            (pos-y (tile-pos-y tile-pos))
            (occlusion-map (state-ref node '(stack occlusion-map))))
        ;; Checking positions in clockwise order from top left
        (not (or (tile-exists? layer (- pos-x 0.5) (- pos-y 0.5) occlusion-map)
                 (tile-exists? layer pos-x         (- pos-y 0.5) occlusion-map)
                 (tile-exists? layer (+ pos-x 0.5) (- pos-y 0.5) occlusion-map)
                 (tile-exists? layer (+ pos-x 0.5) pos-y         occlusion-map)
                 (tile-exists? layer (+ pos-x 0.5) (+ pos-y 0.5) occlusion-map)
                 (tile-exists? layer pos-x         (+ pos-y 0.5) occlusion-map)
                 (tile-exists? layer (- pos-x 0.5) (+ pos-y 0.5) occlusion-map)
                 (tile-exists? layer (- pos-x 0.5) pos-y         occlusion-map)))))

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

    ;; -- Component definition -------------------------------------------------

    (define (stack-component)
      (make-component stack
        (tiles         '())
        (playable      '())
        (match-pairs   '())
        (selected-tile #f)
        (tile-count    0)
        (initial-stack '())
        (layer-opacity #f)  ;; Cons of first layer and opacity
        (occlusion-map (make-hamt))
        (handlers      (add-method `(stack ,@stack-handler)))
        (renderers     (add-method `(stack ,@stack-renderer)))))))
