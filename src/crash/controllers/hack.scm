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

(define-library (crash controllers hack)
  (import (gambit)
          (substratic engine state)
          (substratic engine events)
          (substratic engine keyboard)
          (substratic engine components component))
  (export hack-controller-component)
  (begin

    (define (hack-controller-handler node context event event-sink)
      (case (event-type event)
       ((keyboard)
        (handle-key event
          (case-key
            ;; Horizontal selection
            ("a" (println "Pressed A"))
            ("s" (println "Pressed S"))
            ("d" (println "Pressed D"))
            ("f" (println "Pressed F"))

            ;; Vertical selection
            ("j" (println "Pressed J"))
            ("k" (println "Pressed K"))
            ("l" (println "Pressed L"))
            (";" (println "Pressed ;")))))))

    (define (hack-controller-component)
      (make-component hack
        (handlers      (add-method `(hack ,@hack-controller-handler)))))))
