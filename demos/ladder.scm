(define *demo-debug* #t)
(define *debug* #f)

(declare-form 'left-hand '3D-hand-form)
(declare-form 'right-hand '3D-hand-form)
(declare-form 'rung '3D-form)

(define hands-far-away (make-basic-constraint
                         '(left-hand right-hand)
                         (lambda (h1 h2)
                           0.5)
                         (lambda (h1 h2)
                           (list (cons h1 (list (list 'vertices (make-vertex 1 2 3))))
                                 (cons h2 (list (list 'vertices (make-vertex 4 5 6))))))))

(define hands-end-of-rung (make-basic-constraint
                            '(left-hand right-hand)
                            (lambda (h1 h2)
                              0.5)
                            (lambda (h1 h2)
                           (list (cons h1 (list (list 'vertices (make-vertex 1 2 3))))
                                 (cons h2 (list (list 'vertices (make-vertex 4 5 6))))))))

(define hands-on-ladder (make-compound-constraint
                          (list hands-far-away hands-end-of-rung)
                          (lambda (hfa heor)
                            (let ((h1 (hfa))
                                  (h2 (heor)))
                              (and (> h1 .25) (> h2 .25))))))

(display "Final hands-on-ladder value:")(write (hands-on-ladder))(newline)

(basic-iterative-solver '(left-hand right-hand) '(hands-end-of-rung hands-far-away))
;;(iterative-solver '(left-hand right-hand) '(hands-on-ladder))
