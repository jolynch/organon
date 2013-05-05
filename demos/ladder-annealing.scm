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
              (make-frame (make-vertex -1 -8 -3) (make-quaternion 0 0 0 1)))

(set-property 'right-hand 'vertices (list (make-vertex 2 -1 0)
                                          (make-vertex 2 1 1)
                                          (make-vertex 0 -1 0)
                                          (make-vertex 0 1 0)))

(set-property 'right-hand 'frame
              (make-frame (make-vertex -1 -4 -1) (make-quaternion 0 0 0 1)))

(set-property 'rung 'left-rung (make-vertex -10 0 0))
(set-property 'rung 'right-rung (make-vertex 10 0 0))
(set-property 'rung 'radius 1.0)

(set-property 'desired-distance 'value 5)
(set-property 'axis 'value (make-vector 1 0 0))
(set-property 'desired-closeness 'value .1)

(define hands-far-away
  (make-basic-constraint
    '(left-hand right-hand desired-distance)
    (lambda (left-hand right-hand d)
      (cond ((and (is-type? left-hand '3D-hand-form)
                  (is-type? right-hand '3D-hand-form)
                  (is-type? d 'basic))
             (let* ((left-origin (car (get-property left-hand 'frame)))
                    (right-origin (car (get-property right-hand 'frame)))
                    (dis (distance left-origin right-origin)))
               (min 1.0 (/ dis (get-value d)))))
            (else 0.0)))
    (lambda (left-hand right-hand d)
      (let* ((left-hand-old (get-property left-hand 'frame))
             (right-hand-old (get-property right-hand 'frame))
             (left-hand-vector (frame-vector left-hand-old))
             (right-hand-vector (frame-vector right-hand-old))
             (left-hand-quat (frame-quat left-hand-old))
             (right-hand-quat (frame-quat right-hand-old))
             (left-hand-inverted (scale-vector
                                   (unit
                                     (sub-vector right-hand-vector left-hand-vector))
                                   -1))
             (right-hand-inverted (scale-vector
                                    (unit
                                      (sub-vector left-hand-vector right-hand-vector))
                                   -1)))
        (make-binding-list
          (make-binding
            left-hand
            (list 'frame (make-frame (add-vector left-hand-vector left-hand-inverted)
                                     left-hand-quat)))
          (make-binding
            right-hand
            (list 'frame (make-frame (add-vector right-hand-vector right-hand-inverted)
                                     right-hand-quat))))))))

(define hands-end-of-rung
  (make-basic-constraint
    '(left-hand right-hand desired-closeness rung)
    ;; Constraint is that the left hand should be near the left rung goal
    ;; and the right hand should be near the right rung goal
    (lambda (left-hand right-hand d rung)
      (cond
        ((and (is-type? left-hand '3D-hand-form)
              (is-type? right-hand '3D-hand-form)
              (is-type? d 'basic)
              (is-type? rung 'rungt))
         (let* ((left-origin (car (get-property left-hand 'frame)))
                (right-origin (car (get-property right-hand 'frame)))
                (left-rung (get-property rung 'left-rung))
                (right-rung (get-property rung 'right-rung))
                (disl (distance left-origin left-rung))
                (disr (distance right-origin right-rung)))
           (min
             1.0
             (+ (/ (min 1.0 (/ (get-value d) disl)) 2.0)
                (/ (min 1.0 (/ (get-value d) disr)) 2.0)))))
        (else 0.0)))
    ;; Hint generates four possible solution points between each hand and
    ;; the goal rung
    (lambda (left-hand right-hand d rung)
      (let* ((left-hand-old (get-property left-hand 'frame))
             (right-hand-old (get-property right-hand 'frame))
             (left-hand-quat  (frame-quat left-hand-old))
             (right-hand-quat  (frame-quat right-hand-old))
             (left-hand-vector  (frame-vector left-hand-old))
             (right-hand-vector  (frame-vector right-hand-old))
             (left-hand-rung  (get-property rung 'left-rung))
             (right-hand-rung  (get-property rung 'right-rung))
             (left-hand-goal  (interpolate left-hand-vector left-hand-rung 5))
             (right-hand-goal  (interpolate right-hand-vector right-hand-rung 5))
             (left-hand-goal-frame
               (map (lambda (goal)
                      (list 'frame (make-frame goal left-hand-quat))) left-hand-goal))
             (right-hand-goal-frame
               (map (lambda (goal)
                      (list 'frame (make-frame goal right-hand-quat))) right-hand-goal)))
        (join-lists (list (apply make-binding-list
                                 (map (lambda (goal)
                                        (make-binding left-hand goal))
                                      left-hand-goal-frame))
                          (apply make-binding-list
                                 (map (lambda (goal)
                                        (make-binding right-hand goal))
                                      right-hand-goal-frame))))))))

(define hands-on-ladder
  (make-compound-constraint
    (list hands-far-away hands-end-of-rung)
    (lambda (hfa heor)
      (let ((h1 (hfa))
            (h2 (heor)))
        (/ (+ h1 h2) 2.0)))))

(pp "Final hands-on-ladder value:")(write (hands-on-ladder))(newline)

(basic-annealing-solver '(left-hand right-hand) (list hands-on-ladder))

;;(iterative-solver '(left-hand right-hand) '(hands-on-ladder))

;; Two constraints, each with two hints - 4 hints total. 2^4 = 16 subsets. And
;; indeed, we see 16 output scores. Each score is a list that shows the score we
;; got from applying to first form that was in the hint, then the first two
;; forms that were in the hint, then the first three forms, etc.
