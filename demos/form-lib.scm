;; ================================================================================
;; form declarations
;; ================================================================================
(declare-form-type '3D-form (list 'frame 'vertices))

(declare-type-inherits 'hand-form '3D-form)
(declare-form-type 'hand-form (list 'palm-width))

(declare-type-inherits '3D-hand-form 'hand-form)
(declare-form-type '3D-hand-form (list 'finger1-box 'finger2-box 'finger3-box 'finger4-box 'finger5-box))

(declare-form-type 'basic (list 'value))

(define (get-value f)
  (get-property f 'value))

;; ================================================================================
;; form instantiations
;; ================================================================================
(define (make-vector x y z) (list x y z))
(define make-vertex make-vector)
(define (make-quaternion q1 q2 q3 q4) (list q1 q2 q3 q4))
(define (make-frame vector quaternion) (list vector quaternion))
(define (frame-vector f) (car f))
(define (frame-quat f) (cadr f))

;; Vector convenience methods

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



;; (declare-form 'thing1 '3D-form)
;; (make-form 'thing1 '3D-form
;;             (list (make-frame (make-vector 0 0 0) (make-quaternion 0 0 0 0))
;;                   (make-vertex 0 0 0)))
;; 
;; (declare-form 'lhand '3D-hand-form)
 
;; (pp-form 'thing1)
;; (pp-form 'lhand)
;; (pp (capture-bindings 'thing1))
;; (apply-bindings 'thing1 (capture-bindings 'thing1))
;; (pp (capture-bindings 'thing1)) ;; should be the same

;; (make-3d-form 'thing1)
;; (make-3d-form 'thing2)
;; (make-3d-hand-form 'lhand)
;; (pp (has-property? 'thing1 'frame))
;; (pp (has-property? 'thing1 'frame))
;; (pp (same-type? 'thing1 'thing2))
;; (pp (same-type? 'lhand 'thing2))
