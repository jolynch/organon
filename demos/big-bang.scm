(define *demo-debug* #t)
(define *debug* #f)

(for-each (lambda (i) (declare-form (symbol 'star- i) 'star)) (range 0 100))

(declare-form 'desired-distance 'basic)

;; Set up the initial conditions

(set-property 'desired-distance 'value 10)

(for-each (lambda (i)
            (set-property (symbol 'star- i) 'radius 1)) (range 0 100))

(for-each (lambda (i)
            (set-property (symbol 'star- i)
                          'center
                          (make-vertex (random 1.0)
                                       (random 1.0)
                                       (random 1.0)))) (range 0 100))

(define (universe-constraint d . forms)
  (let* ((avgx (/ (apply + (map (lambda (f) (vx (get-property f 'center))) forms))
                  (length forms)))
         (avgy (/ (apply + (map (lambda (f) (vy (get-property f 'center))) forms))
                  (length forms)))
         (avgz (/ (apply + (map (lambda (f) (vz (get-property f 'center))) forms))
                  (length forms)))
         (center-of-mass (make-vector avgx avgy avgz)))
    (define (score-single form)
      (let* ((pos (get-property form 'center))
             (dis (distance center-of-mass pos)))
        (min 1.0 (/ dis (get-value d)))))
    (/ (apply + (map score-single forms)) (length forms))))

(define (universe-hint d . forms)
  (let* ((avgx (/ (apply + (map (lambda (f) (vx (get-property f 'center))) forms))
                  (length forms)))
         (avgy (/ (apply + (map (lambda (f) (vy (get-property f 'center))) forms))
                  (length forms)))
         (avgz (/ (apply + (map (lambda (f) (vz (get-property f 'center))) forms))
                  (length forms)))
         (center-of-mass (make-vector avgx avgy avgz)))
    (define (hint-single form)
      (let* ((pos (get-property form 'center))
             (pos-inverted (unit (sub-vector pos center-of-mass))))
        (make-binding form (list 'center (add-vector pos pos-inverted)))))
    (map hint-single forms)))

(define universe-exploded
  (make-basic-constraint
    (cons 'desired-distance (map (lambda (i) (symbol 'star- i)) (range 0 100)))
    universe-constraint
    universe-hint))

(pp "making connection")
(make-connection)

(basic-annealing-solver (map (lambda (i) (symbol 'star- i)) (range 0 100))
                        (list universe-exploded)
                        100)

(close-connection)

;;(iterative-solver '(left-hand right-hand) '(hands-on-ladder))

;; Two constraints, each with two hints - 4 hints total. 2^4 = 16 subsets. And
;; indeed, we see 16 output scores. Each score is a list that shows the score we
;; got from applying to first form that was in the hint, then the first two
;; forms that were in the hint, then the first three forms, etc.
