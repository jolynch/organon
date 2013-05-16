(define *use-network-visualizer* #f)

(declare-form-type 'node (list 'color))
(declare-form 'colors 'basic)
(set-property 'colors 'value '(red blue green))

(declare-form 'A 'node)
(declare-form 'B 'node)
(declare-form 'C 'node)
(declare-form 'D 'node)

(define (Node-constraint x y)
  (if (not (eq? (get-property x 'color) (get-property y 'color)))
    1.0
    0.0))

(define (Node-hint x y)
  (let ((x-color (get-property x 'color))
        (y-color (get-property y 'color)))
    (if (null? x-color)
      (make-binding-list
        (make-binding x (list 'color (car (get-value 'colors)))))
      (cond
        ((and (eq? x-color y-color) (> (random 5) 2))
         (make-binding-list))
        (else
          (make-binding-list
            (random-choice
              (map (lambda (color)
                     (make-binding y (list 'color color)))
                   (filter (lambda (z) (not (eq? z x-color))) (get-value 'colors))))))))))


;; Graph of the form
;;  A
;;  |\
;;  | C -- D
;;  |/
;;  B
;;
;;  Need to define a constraint for each pair of nodes
(define AB (make-basic-constraint '(A B) Node-constraint Node-hint))
(define AC (make-basic-constraint '(A C) Node-constraint Node-hint))
(define BC (make-basic-constraint '(B C) Node-constraint Node-hint))
(define CD (make-basic-constraint '(C D) Node-constraint Node-hint))

(define all-colored
  (make-compound-constraint
    (list AB AC BC CD)
    (lambda (one two three four)
      (let ((c1 (one))
            (c2 (two))
            (c3 (three))
            (c4 (four)))
      (/ (+ c1 c2 c3 c4) 4.0)))))

(basic-annealing-solver '(A B C D) (list all-colored) 1000)

