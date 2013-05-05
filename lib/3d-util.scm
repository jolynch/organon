;; 3d form convenience methods
(define (make-vector x y z) (list x y z))
(define make-vertex make-vector)
(define (make-quaternion q1 q2 q3 q4) (list q1 q2 q3 q4))
(define (make-frame vector quaternion) (list vector quaternion))
(define (frame-vector f) (car f))
(define (frame-quat f) (cadr f))

;; Vector convenience methods
(define (distance v1 v2)
  (if (and (eq? (length v1) 3) (eq? (length v2) 3))
  (sqrt (+ (square (- (first v1) (first v2)))
           (square (- (second v1) (second v2)))
           (square (- (third v1) (third v2)))))
  0.0))

(define (vx v) (first v))
(define (vy v) (second v))
(define (vz v) (third v))

(define (add-vector v1 v2)
  (make-vector (+ (vx v1) (vx v2))
               (+ (vy v1) (vy v2))
               (+ (vz v1) (vz v2))))

(define (add-constant v1 c)
  (make-vector (+ (vx v1) c)
               (+ (vy v1) c)
               (+ (vz v1) c)))

(define (scale-vector v1 s)
  (make-vector (* (vx v1) s)
               (* (vy v1) s)
               (* (vz v1) s)))

;; V1 - V2
(define (sub-vector v1 v2)
  (make-vector (- (vx v1) (vx v2))
               (- (vy v1) (vy v2))
               (- (vz v1) (vz v2))))

(define (make-exact v1)
  (map inexact->exact v1))

(define (mag v1)
  (distance v1 '(0 0 0)))

(define (range x y)
  (define (iter x y ls)
    (cond
      ((< x y)
       (iter (+ x 1) y (cons x ls)))
      (else ls)))
  (reverse (iter x y '())))

(define (unit v1)
  (scale-vector v1 (/ 1.0 (mag v1))))

(define (interpolate x y n)
  (define (fake-inter v1 v2 num)
    (let* ((direction (sub-vector v2 v1))
           (udir (unit direction))
           (scale (/ (distance v1 v2) (- num 1))))
      (map (lambda (n)
             (add-vector v1 (scale-vector udir (* n scale))))
           (range 0 num))))
  (reverse (cdr (reverse (cdr (fake-inter x y (+ 2 n)))))))


