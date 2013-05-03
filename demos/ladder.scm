(define *demo-debug* #t)
(define *debug* #f)

(declare-form 'left-hand '3D-hand-form)
(declare-form 'right-hand '3D-hand-form)
(declare-form 'rung '3D-form)
(declare-form 'desired-distance 'basic)

(define hands-far-away (make-basic-constraint
                         '(left-hand right-hand desired-distance)
                         (lambda (lh rh d)
                           (if (and (is-type? lh '3D-hand-form)
                                    (is-type? rh '3D-hand-form)
                                    (is-type? d 'basic))
                             0.5
                             0.0))
                         (lambda (h1 h2 d)
                           (list (cons h1 (list (list 'vertices (make-vertex 2 4 8))))
                                 (cons h2 (list (list 'vertices (make-vertex 8 4 2))))))))

(define hands-end-of-rung (make-basic-constraint
                            '(left-hand right-hand)
                            (lambda (h1 h2)
                              0.5)
                            (lambda (h1 h2)
                           (list (cons h1 (list (list 'vertices (make-vertex 1 3 5))))
                                 (cons h2 (list (list 'vertices (make-vertex 5 3 1))))))))

(define hands-on-ladder (make-compound-constraint
                          (list hands-far-away hands-end-of-rung)
                          (lambda (hfa heor)
                            (let ((h1 (hfa))
                                  (h2 (heor)))
                              (and (> h1 .25) (> h2 .25))))))

(display "Final hands-on-ladder value:")(write (hands-on-ladder))(newline)

(basic-iterative-solver '(left-hand right-hand) (list hands-on-ladder))

;;(iterative-solver '(left-hand right-hand) '(hands-on-ladder))
