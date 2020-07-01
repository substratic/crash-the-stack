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

(import (_geiser)
        (substratic engine assets)
        (substratic engine config)
        (substratic engine logging)
        (crash main))

;; Configure development environment
(assets-base-path-set! "./dist/assets")
(render-colliders-set! #f)

;; Drop into a REPL before startup if requested
(if (member "--repl" (command-line))
    (##repl))

;; Look for --editor command
(define initial-mode
  (let loop ((args (cdr (command-line))))
    (if (pair? args)
        (cond
          ((equal? "--editor" (car args))
           `(edit . ,(cadr args)))
          ((equal? "--stack" (car args))
           `(game . ,(cadr args)))
          (else (loop (cdr args))))
        #f)))

;; Start it up!
(main start-forge?: #t
      initial-mode: initial-mode
      debug: #t)
