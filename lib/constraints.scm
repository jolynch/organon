(define *debug* #t)

;; Mapping of forms to dependent constraints
(define *forms* (make-eq-hash-table))

;; Mapping of constrains to dependent constraints
(define *constraints* (make-eq-hash-table))

(define (show-form-mapping)
  (pp (cdr (eq-plist *forms*))))

(define (register-with node name item)
  (let ((val (eq-get node item)))
    (cond (val  (eq-put! node item (cons name val)))
          (else (eq-put! node item (list name))))))

(define (register-form name form)
  (register-with *forms* name form))

(define (register-constraint name constraint)
  (register-with *constraints* name constraints))

(define (make-basic-constraint forms func)
  (define me
    (make-entity
      (lambda (self . args)
        (cond ((null? args) (apply func forms))
              (else (apply func args))))
      'no-extra))
  (for-each (lambda (form) (register-form me form)) forms)
  me)

(define (make-compound-constraint constraints func)
  (define me
    (make-entity
      (lambda (self . args)
        (cond ((null? args) (apply func constraints))
              (else (apply func args))))
      'no-extra))
  (for-each (lambda (constraint) (register-constraint me constraint)) constraints)
  me)

;; E.G.
;; (define x (make-basic-constraint '(foo bar) (lambda (f1 f2) (display f1)(display " ")(display f2))))


