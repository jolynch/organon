(define *debug* #t)

;; Mapping of forms to dependent constraints
(define *forms* (make-eq-hash-table))

;; Mapping of constrains to dependent constraints
(define *constraints* (make-eq-hash-table))

(define (update-form form)
  (let ((constraints (eq-get *forms* form)))
    (if constraints
      (for-each (lambda (constraint)
                  (constraint))
                constraints))))

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
          ((eq? (car (entity-extra self)) 'dirty)
           (pp "Evaluating basic constraint")
           (let ((value
                   (if (null? args)
                     (apply func forms)
                     (apply func args))))
             (set-entity-extra! self (cons 'clean value))
             value))
          (else
            (display "Using cached value")
            (cdr (entity-extra self)))))
      '(dirty)))
  (for-each (lambda (form) (register-form me form)) forms)
  me)

(define (make-compound-constraint constraints func)
  (define me
    (make-entity
      (lambda (self . args)
        (cond
          ((eq? (car (entity-extra self)) 'dirty)
           (let ((value
                   (if (null? args)
                     (apply func constraints)
                     (apply func args))))
             (set-entity-extra! self (cons 'clean value))
             value))
          (else
            (display "Using cached value")
            (cdr (entity-extra self)))))
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


