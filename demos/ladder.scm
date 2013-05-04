(define *demo-debug* #t)
(define *debug* #f)

(declare-form 'left-hand '3D-hand-form)
(declare-form 'right-hand '3D-hand-form)
(declare-form 'rung 'rungt)
(declare-form 'desired-distance 'basic)
(declare-form 'desired-closeness 'basic)
(declare-form 'axis 'basic)

;; Set up the initial conditions

(set-property 'left-hand 'vertices (list (make-vertex -2 -1 0)
                                         (make-vertex -2 1 1)
                                         (make-vertex 0 -1 0)
                                         (make-vertex 0 1 0)))

(set-property 'left-hand 'frame
              (make-frame (make-vertex -1 0 0) (make-quaternion 0 0 0 1)))

(set-property 'right-hand 'vertices (list (make-vertex 2 -1 0)
                                          (make-vertex 2 1 1)
                                          (make-vertex 0 -1 0)
                                          (make-vertex 0 1 0)))

(set-property 'right-hand 'frame
              (make-frame (make-vertex 1 0 0) (make-quaternion 0 0 0 1)))

(set-property 'rung 'left-rung (make-vertex -10 0 0))
(set-property 'rung 'right-rung (make-vertex 10 0 0))
(set-property 'rung 'radius 1.0)

(set-property 'desired-distance 'value 5)
(set-property 'axis 'value (make-vector 1 0 0))
(set-property 'desired-closeness 'value .5)

(define hands-far-away
  (make-basic-constraint
    '(left-hand right-hand desired-distance)
    (lambda (lh rh d)
      (cond ((and (is-type? lh '3D-hand-form)
                  (is-type? rh '3D-hand-form)
                  (is-type? d 'basic))
             (let* ((left-origin (car (get-property lh 'frame)))
                    (right-origin (car (get-property rh 'frame)))
                    (dis (distance left-origin right-origin)))
               (pp left-origin)
               (pp right-origin)
               (pp dis)
               (min 1.0 (/ dis (get-value d)))))
            (else 0.0)))
    (lambda (lh rh d)
      (let* ((lhof (get-property lh 'frame))
             (rhof (get-property rh 'frame))
             (lhq (frame-quat lhof))
             (rhq (frame-quat rhof)))
        (make-binding-list)))))

(define hands-end-of-rung
  (make-basic-constraint
    '(left-hand right-hand desired-closeness rung)
    (lambda (lh rh d r)
      (cond
        ((and (is-type? lh '3D-hand-form)
              (is-type? rh '3D-hand-form)
              (is-type? d 'basic)
              (is-type? r 'rungt))
         (let* ((left-origin (car (get-property lh 'frame)))
                (right-origin (car (get-property rh 'frame)))
                (left-rung (get-property r 'left-rung))
                (right-rung (get-property r 'right-rung))
                (disl (distance left-origin left-rung))
                (disr (distance right-origin right-rung)))
           (min
             1.0
             (+ (/ (min 1.0 (/ (get-value d) disl)) 2.0)
                (/ (min 1.0 (/ (get-value d) disr)) 2.0)))))
        (else 0.0)))
    (lambda (lh rh d r)
      (let* ((lhof (get-property lh 'frame))
             (rhof (get-property rh 'frame))
             (lhq  (frame-quat lhof))
             (rhq  (frame-quat rhof))
             (lhv  (frame-vector lhof))
             (rhv  (frame-vector rhof))
             (lhr  (get-property r 'left-rung))
             (rhr  (get-property r 'right-rung))
             (lhg  (interpolate lhv lhr 3))
             (rhg  (interpolate rhv rhr 3))
             (lhgf (map (lambda (goal) (list 'frame (make-frame goal lhq))) lhg))
             (rhgf (map (lambda (goal) (list 'frame (make-frame goal rhq))) rhg)))
        (join-lists (list (apply make-binding-list (map (lambda (goal) (make-binding lh goal)) lhgf))
                          (apply make-binding-list (map (lambda (goal) (make-binding rh goal)) rhgf))))))))

(define hands-on-ladder
  (make-compound-constraint
    (list hands-far-away hands-end-of-rung)
    (lambda (hfa heor)
      (let ((h1 (hfa))
            (h2 (heor)))
        (/ (+ h1 h2) 2.0)))))

(pp "Final hands-on-ladder value:")(write (hands-on-ladder))(newline)

(basic-iterative-solver '(left-hand right-hand) (list hands-on-ladder))

;;(iterative-solver '(left-hand right-hand) '(hands-on-ladder))

;; Two constraints, each with two hints - 4 hints total. 2^4 = 16 subsets. And
;; indeed, we see 16 output scores. Each score is a list that shows the score we
;; got from applying to first form that was in the hint, then the first two
;; forms that were in the hint, then the first three forms, etc.
