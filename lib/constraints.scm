(define *debug* #t)

;; Mapping of forms to dependent constraints
(define *forms* (make-eq-hash-table))

;; Mapping of constrains to dependent constraints
(define *constraints* (make-eq-hash-table))

;; Convenience methods to deal with entity state
(define (dirty? entity)
  (eq? (car (entity-extra entity)) 'dirty))

(define (make-dirty entity)
  (set-entity-extra! entity '(dirty)))

(define (make-clean entity value)
  (set-entity-extra! entity (cons 'clean value)))

(define (get-value entity)
  (cdr (entity-extra entity)))

;; Methods to cause propagation
(define (update-form form)
  (let ((constraints (eq-get *forms* form)))
    (propagate-to '() constraints)))

;; Causes all dependent constraints to fire
(define (propagate constraint)
  (let ((constraints (eq-get *constraints* constraint)))
    (propagate-to constraint constraints)))

;; Actually call the dependent constraints
;; will need to pass list of calling dependencies to detect loops
(define (propagate-to orig-constraint constraints)
  (if constraints
    (for-each (lambda (constraint)
                (if (not (eq? orig-constraint constraint))
                  (make-dirty constraint))
                (constraint))
              constraints)))

(define (show-form-mapping)
  (pp (cdr (eq-plist *forms*))))

(define (register-with node name item)
  (let ((val (eq-get node item)))
    (cond (val  (eq-put! node item (cons name val)))
          (else (eq-put! node item (list name))))))

(define (register-form name form)
  (register-with *forms* name form))

(define (register-constraint name constraint)
  (register-with *constraints* name constraint))

(define (make-basic-constraint forms func)
  (define me
    (make-entity
      (lambda (self . args)
        (cond
          ((dirty? self)
           (pp "Evaluating basic constraint")
           (let ((value
                   (if (null? args)
                     (apply func forms)
                     (apply func args))))
             (make-clean self value)
             ;; Very important to mark self clean before propagating up
             (propagate self)
             value))
          (else
            (display "Using cached value for ")(write self)(newline)
            (get-value self))))
      '(dirty)))
  (for-each (lambda (form) (register-form me form)) forms)
  me)

(define (make-compound-constraint constraints func)
  (define me
    (make-entity
      (lambda (self . args)
        (cond
          ((dirty? self)
           (pp "Evaluating compound constraint")
           (let ((value
                   (if (null? args)
                     (apply func constraints)
                     (apply func args))))
             (make-clean self value)
             (propagate self)
             value))
          (else
            (display "Using cached value for ")(write self)(newline)
            (get-value self))))
      '(dirty)))
  (for-each (lambda (constraint) (register-constraint me constraint)) constraints)
  me)

;; Some tests

(declare-form 'left-hand '3D-hand-form)
(declare-form 'right-hand '3D-hand-form)
(declare-form 'rung '3D-form)

(define hands-far-away (make-basic-constraint
                         '(left-hand right-hand)
                         (lambda (h1 h2)
                           0.5)))

(define hands-end-of-rung (make-basic-constraint
                            '(left-hand right-hand)
                            (lambda (h1 h2)
                              0.5)))

(define hands-on-ladder (make-compound-constraint
                          (list hands-far-away hands-end-of-rung)
                          (lambda (hfa heor)
                            (let ((h1 (hfa))
                                  (h2 (heor)))
                              (and (> h1 .25) (> h2 .25))))))

(hands-on-ladder)
;; (define x (make-basic-constraint '(foo bar) (lambda (f1 f2) (display f1)(display " ")(display f2))))


