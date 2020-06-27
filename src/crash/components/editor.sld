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

(define-library (crash components editor)
  (import (gambit)
          (crash tile)
          (crash stack)
          (crash controllers wsad)
          (substratic sdl2)
          (substratic engine node)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine events)
          (substratic engine logging)
          (substratic engine keyboard)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components)
          (substratic engine components messages))
  (export editor-component)

  (begin

    (define (update-layer-opacity node layer)
      (let* ((layer-opacity (state-ref node '(editor layer-opacity)))
             (layer-opacity (if layer-opacity
                                (cons (+ layer 1) (cdr layer-opacity))
                                #f)))
        (update-state node (editor (> (layer-opacity layer-opacity)))
                           (stack (> (layer-opacity layer-opacity))))))

    (define (move-cursor node delta-x delta-y delta-z)
      (let* ((cursor-pos (state-ref node '(editor cursor-pos)))
             (new-pos (list (max 0 (+ delta-z (list-ref cursor-pos 0)))
                            (+ delta-x (list-ref cursor-pos 1))
                            (+ delta-y (list-ref cursor-pos 2)))))
        (set! node (update-layer-opacity node (tile-pos-layer new-pos)))
        (update-state node (editor (> (cursor-pos new-pos)
                                      (can-place? (tile-placeable? node new-pos)))))))

    (define (handle-editor-key node event event-sink)
      (handle-key
       event
       direction: 'up
       (case-key
        ;; Cursor Movement
        ("j"   (set! node (move-cursor node  0    1   0)))
        ("k"   (set! node (move-cursor node  0   -1   0)))
        ("h"   (set! node (move-cursor node -1    0   0)))
        ("l"   (set! node (move-cursor node  1    0   0)))
        ("S-j" (set! node (move-cursor node  0    0.5 0)))
        ("S-k" (set! node (move-cursor node  0   -0.5 0)))
        ("S-h" (set! node (move-cursor node -0.5  0   0)))
        ("S-l" (set! node (move-cursor node  0.5  0   0)))
        ("u"   (set! node (move-cursor node  0    0   1)))
        ("d"   (set! node (move-cursor node  0    0  -1)))

        ;; Tile Placement
        ("SPC"
         (if (state-ref node '(editor can-place?))
             (event-sink (make-event 'stack/insert-or-remove-tile
                                     data: `((tile-pos . ,(state-ref node '(editor cursor-pos))))))
             (print-message event-sink "Can't place here!")))

        ;; Editor Modes
        ("C-p"
          (let ((tile-count (state-ref node '(stack tile-count))))
            (if (and (> tile-count 0)
                     (> (modulo tile-count 2) 0))
                (print-message event-sink "Can't play a stack with odd tile count")
                (begin
                  (event-sink (make-event 'stack/store-initial))
                  (event-sink (make-event 'stack/shuffle))
                  (event-sink (make-event 'game/unpause))
                  (update-state node (editor (> (mode 'playback)))
                                     (stack  (> (layer-opacity #f))))))))

        ;; Layer Visibility
        ("TAB"
         (let* ((layer-opacity (state-ref node '(editor layer-opacity)))
                (layer-opacity (if layer-opacity
                                   (if (equal? (cdr layer-opacity) 70)
                                       (cons (car layer-opacity) 0)
                                       #f)
                                   (cons (+ (tile-pos-layer (state-ref node '(editor cursor-pos))) 1)
                                         70))))
           (update-state node (editor (> (layer-opacity layer-opacity)))
                              (stack  (> (layer-opacity layer-opacity))))))

        ;; File Operations
        ("C-s"
          (let ((tile-count (state-ref node '(stack tile-count))))
            (if (> tile-count 0)
                (if (> (modulo tile-count 2) 0)
                    (print-message event-sink "Can't save stack with odd number of tiles!")
                    (event-sink (make-event 'stack/save
                                            data: `((stack-file . ,(state-ref node '(editor stack-file)))))))
                (print-message event-sink "No tiles to save!")))))))

    (define (editor-handler node context event event-sink)
      (case (event-type event)
        ((keyboard)
         (if (equal? (state-ref node '(editor mode)) 'editor)
             (handle-editor-key node event event-sink)
             (handle-key
              event
              direction: 'up
              (case-key
               ("C-p"
                 (event-sink (make-event 'stack/reset))
                 (event-sink (make-event 'game/pause))
                 (update-state node (editor (> (mode 'editor)))
                                    (stack  (> (layer-opacity (state-ref node '(editor layer-opacity)))))))))))))

    (define (editor-updater node context time-step event-sink)
      (if (state-ref node '(editor panel))
          (update-state node
                        (editor (> (panel (lambda (panel)
                                            (update-node panel time-step event-sink))))))
          node))

    (define (editor-renderer node context renderer)
      (with-state node ((editor mode visible cursor-pos can-place?)
                        (stack tile-count))
        (let ((screen-width  (state-ref context 'screen-width))
              (screen-height (state-ref context 'screen-height)))

          (render-fill-rect renderer
                            (list 0 0 23 13)
                            (if (equal? mode 'editor)
                                (make-color 204 255 51)
                                (make-color 255 80 80)))
          (render-text renderer
                       (if (equal? mode 'editor) "EDIT" "PLAY")
                       *default-font-small*
                       2 2
                       color: (make-color 0 0 0))

          (render-text renderer
                        (string-append "Tiles: " (number->string tile-count))
                        *default-font-small*
                        (- screen-width 10)
                        (- screen-height 27)
                        align: 'right
                        color: (if (and (> tile-count 0)
                                        (> (modulo tile-count 2) 0))
                                   (make-color 255 80 80)
                                   (make-color 255 255 255)))

          (render-text renderer
                       (string-append "Layer: "
                                      (number->string (car cursor-pos))
                                      " - x: "
                                      (number->string (cadr cursor-pos))
                                      " / y: "
                                      (number->string (caddr cursor-pos)))
                       *default-font-small*
                       (- screen-width 10)
                       (- screen-height 15)
                       align: 'right
                       color: (make-color 255 255 255))

          (when (equal? mode 'editor)
            (render-rect
              renderer
              (tile-pos->screen-rect
                cursor-pos
                screen-width
                screen-height)
              (if can-place?
                  (make-color 0 255 0)
                  (make-color 255 0 0)))))))

    (define (editor-component #!key (stack-file #f))
      (make-component
       editor
       (visible       #t)
       (mode          'editor) ;; Or 'playback
       (cursor-pos    '(0 0. 0.))
       (can-place?    #t)
       (stack-file    stack-file)
       (layer-opacity #f)
       (updaters      (add-method `(editor ,@editor-updater)))
       (handlers      (add-method `(editor ,@editor-handler)))
       (renderers     (add-method `(editor . crash/components/editor#editor-renderer)))))))
