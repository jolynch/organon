(define (has-property? form property)
  (if (get-property form property) #t #f))

(define (get-property form property)
  (eq-get form property))

;; TODO - decide whether to convert this to a continuation form rather than calling update-form immediately
(define (set-property form property value)
  (let ((val (eq-put! form property value)))
    (update-form form)
    val))

(define (capture-bindings form)
  (eq-plist-simple form))

(define (apply-bindings form bindings)
  (for-each (lambda (binding) (set-property form (car binding) (cdr binding))) bindings))

(define (same-type? form-a form-b)
  (equal? (eq-ordered-plist form-a) (eq-ordered-plist form-b)))

;; Tells if a form implements the interface provided by type
(define (is-type? form type)
  (define (implements? form type)
    (if (eq-get 'form-types type)
      (let ((desired-properties (eq-get 'form-types type)))
        (reduce (lambda (x y) (and x y)) #t
                (map (lambda (x) (has-property? form x)) desired-properties)))
      #f))
  (reduce (lambda (x y) (and x y)) #t
          (map (lambda (x) (implements? form x)) (all-parents type))))

;; Gets all parent types for a given type
(define (all-parents type)
  (let find-parents ((result (list type))
                     (ntype type))
    (if (eq-get 'form-inherits ntype)
      (find-parents (cons (eq-get 'form-inherits ntype) result) (eq-get 'form-inherits ntype))
      result)))

;; Check if a form is many types
(define (is-multiple-type? form types)
  (reduce (lambda (x y) (and x y)) #t
          (map (lambda (x) (is-type? form x)) types)))


(define (pp-form form) (pp (capture-bindings form)))

;; TODO
;;(define (sub-type? form-a form-b)
;; 
;;)

;; makes a form from a properties-and-values, a list of tuples (name, value)
(define (make-form-by-properties name properties-and-values)
  (for-each (lambda (x)
     (let ( (property-name (first x)) (property-value (second x)) )
      (eq-put! name property-name property-value)
      )) properties-and-values))

;; declare-form-by-properties - declares the form and initializes all the
;; properties to null (ie the empty list)
(define (declare-form-by-properties name properties)
 (make-form-by-properties name (map (lambda (x) (list x '())) properties)))

;; register a type with a list of properties
(define (declare-form-type type properties) (eq-put! 'form-types type properties))

(define (make-form name type properties)
  (make-form-by-properties name (zip (eq-get 'form-types type) properties)))

;; declaring a form creates its superclass properties. making a form does not.
(define (declare-form name type)
  (if (not (equal? #f type))
    (let ( (parent-type (eq-get 'form-inherits type)) 
           (my-type (eq-get 'form-types type)) )
      (if (not (equal? parent-type #f)) (declare-form name parent-type))
      (declare-form-by-properties name my-type))))

(define (declare-type-inherits type-a type-b)
  (eq-put! 'form-inherits type-a type-b))

;; ================================================================================
;; form declarations (will be moved to demo folder soon)
;; ================================================================================
(declare-form-type '3D-form (list 'frame 'vertices))

(declare-type-inherits 'hand-form '3D-form)
(declare-form-type 'hand-form (list 'palm-width))

(declare-type-inherits '3D-hand-form 'hand-form)
(declare-form-type '3D-hand-form (list 'finger1-box 'finger2-box 'finger3-box 'finger4-box 'finger5-box))

(declare-form-type 'basic (list 'value))

;; ================================================================================
;; instantiations (will be moved to demo folder soon)
;; ================================================================================
(define (make-vector x y z) (list x y z))
(define make-vertex make-vector)
(define (make-quaternion q1 q2 q3 q4) (list q1 q2 q3 q4))
(define (make-frame vector quaternion) (list vector quaternion))

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
