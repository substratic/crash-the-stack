@; ;; Tell Gambit to pass along remaining command line args to this script

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

(import (github.com/substratic build))

(define crash-the-stack
  (make-project
    name: "Crash The Stack"
    exe-name: "crash-the-stack"
    version: "0.1.0"

    output-path:
      (lambda (target)
        (if (equal? target 'MacOS)
            "./dist/crash-the-stack.app/Contents/MacOS"
            "./dist"))

    search-paths: '("src/")

    files:
      (lambda (mode target)
        (if (equal? mode 'build)
            '("src/release.scm")
            '("src/dev.scm")))

    modules: '(;; Components
               "crash/components/menu"
               "crash/components/editor"

               ;; Controllers
               "crash/controllers/wsad"
               "crash/controllers/hack"
               "crash/controllers/mouse"

               ;; Game-specific code
               "crash/tile"
               "crash/stack"
               "crash/modes/game"
               "crash/modes/title-screen"

               ;; Program entrypoint
               "crash/main")

    test-files: '("src/crash/stack.test.scm")))

(cond
 ((member "--dev" (command-line)) (run-interactively crash-the-stack))
 ((member "--test" (command-line)) (test-project crash-the-stack))
 (else (build-project crash-the-stack)))
