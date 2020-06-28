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

(define-library (crash controllers mouse)
  (import (gambit)
          (substratic engine state)
          (substratic engine events)
          (substratic engine components component))
  (export mouse-controller-component)
  (begin

    (define (mouse-controller-handler node context event event-sink)
      (case (event-type event)
       ((mouse/button)
        (when (and (equal? (event-data event 'button)    'left)
                   (equal? (event-data event 'direction) 'down))
          (event-sink (make-event 'stack/select-at data: `((pos-x . ,(event-data event 'pos-x))
                                                           (pos-y . ,(event-data event 'pos-y)))))))))

    (define (mouse-controller-component)
      (make-component mouse
        (handlers      (add-method `(mouse ,@mouse-controller-handler)))))))
