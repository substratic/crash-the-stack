(import (_test)
        (crash stack)
        (substratic engine state))

(define pyramid
 '((;; Layer 1
    (run -1.0 -1.0 3)
    (run -1.0 -0.0 3)
    (run -1.0  1.0 3))
   (;; Layer 2
    (run -0.5 -0.5 2)
    (run -0.5  0.5 2))
   (;; Layer 3
    (run -0.0 -0.0 1))))

(define stairs
 '((;; Layer 1
    (run -0.0 -2.0 1)
    (run -1.0 -1.0 2)
    (run -2.0 -0.0 3)
    (run -3.0 -0.0 4))))

(test-group "get-playable-tiles"
  (test-group "returns playable tile positions"
    (let* ((stack (load-stack pyramid (make-state))))
      (test-equal '((2 0. 0.))
        (state-ref stack '(stack playable))))))
