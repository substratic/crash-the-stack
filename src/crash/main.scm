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

(define-library (crash main)
  (import (gambit)
          (crash tile)
          (crash modes game)
          (substratic sdl2)
          (substratic engine loop)
          (substratic engine assets)
          (substratic engine config))
  (export main)
  (begin

    (define (make-title-screen-mode)
      (import (crash modes title-screen))
      (title-screen-mode make-game-mode: game-mode))

    (define (main #!key (start-repl #f)
                        (connect-emacs #f)
                        (initial-mode #f)
                        (debug #f))

      (when start-repl
        ;; Start a REPL for interactive development
        (##start-repl-server "localhost:44555")

        ;; Invoke emacsclient to initiate the REPL connection
        (when connect-emacs
          (shell-command "emacsclient -e \"(connect-deft-repl)\"" #t)))

      ;; Initialize SDL
      (if (< (SDL_Init SDL_INIT_VIDEO) 0) (SDL_LogCritical (string-append "Error initializing SDL " (SDL_GetError))))

      ;; Initialize SDL subsystems
      (IMG_Init IMG_INIT_PNG)
      (TTF_Init)

      (let* ((window-width 1280)
             (window-height 720)
             (screen-width 640)  ; Make the logical screen size smaller to scale up
             (screen-height 360) ; game assets for a retro pixel look
             (window (SDL_CreateWindow "Crash the Stack"
                                       SDL_WINDOWPOS_UNDEFINED
                                       SDL_WINDOWPOS_UNDEFINED
                                       window-width
                                       window-height
                                       (bitwise-ior
                                         ;; SDL_WINDOW_FULLSCREEN_DESKTOP
                                         SDL_WINDOW_ALLOW_HIGHDPI
                                         SDL_WINDOW_OPENGL)))
             (renderer #f))

        (if (not window)
            (println "Error creating window: " (SDL_GetError)))

        (set! renderer (SDL_CreateRenderer window -1 (bitwise-ior SDL_RENDERER_ACCELERATED
                                                                  SDL_RENDERER_PRESENTVSYNC)))
                                                                  ;; SDL_RENDERER_TARGETTEXTURE)))

        (SDL_RenderSetIntegerScale renderer SDL_TRUE)
        (SDL_RenderSetLogicalSize renderer screen-width screen-height)

        (if (not renderer)
            (println "Error creating renderer: " (SDL_GetError)))

        ;; Set the global image loader to use the renderer instance
        (image-loader-set! (lambda (image-path)
                            (github.com/substratic/engine/assets#load-image renderer image-path)))

        ;; Preload assets needed everywhere
        (load-default-fonts)

        ;; Load game assets
        (load-tile-assets)

        ;; Start the game loop
        (game-loop renderer
                   (cond
                    ((equal? #f initial-mode)
                     (make-title-screen-mode))
                    ((equal? 'game (car initial-mode))
                     (game-mode stack-file: (cdr initial-mode)))
                    ((equal? 'edit (car initial-mode))
                     (game-mode editor?: #t stack-file: (cdr initial-mode))))
                   screen-width screen-height
                   enable-rpc: connect-emacs
                   show-fps: debug)

        (SDL_DestroyWindow window)
        (SDL_Quit)))))
