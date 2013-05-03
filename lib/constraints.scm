(define *debug* #f)

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


;; Constraints have the following calling convention
;; (constraint) -> if the node is dirty, it will evaluate the constraint,
;; otherwise it will use the value last returned.  This results in a float
;; between 0 and 1
;;
;; (constraint 'hint) -> Returns a list of hints where hints are an assoc list
;; of form (symbol) -> list of bindings (assoc list), which provides suggested
;; improvements to the bindings
;;
;; (constraint 'children) -> Returns a list of the constraints that are
;; children of this constraint

(define (dispatch-constraint type self args operands func hint-func)
  ;; Allows dispatch on suplied type
  (define (test-args pattern)
    (and (not (null? args)) (symbol? (car args)) (eq? (car args) pattern)))

  ;; We should allow people to use hint as a form
  (cond
    ((test-args 'hint)
     (apply hint-func operands))
    ((test-args 'children)
     operands)
    ((test-args 'eval)
     (apply func (cdr args)))
    ((test-args 'type)
     type)
    ((dirty? self)
     (if *debug* (pp "Evaluating constraint"))
     (let ((value (apply func operands)))
       (make-clean self value)
       (propagate self)
       value))
    (else
      (if *debug* (begin (display "Using cached value for ")(write self)(newline)))
      (get-value self))))

(define (make-basic-constraint forms func #!optional hint-func)
  (make-constraint 'form forms func hint-func))

(define (make-compound-constraint constraints func #!optional hint-func)
  (make-constraint 'constraint constraints func hint-func))

(define (make-constraint type operands func #!optional hint-func)
  (define me
    (make-entity
      (lambda (self . args)
        (dispatch-constraint type self args operands func hint-func))
      '(dirty)))
  (let ((register-function (eval (symbol 'register- type) user-initial-environment)))
    (for-each (lambda (operand) (register-function me operand)) operands))
  me)


;; Some tests

;; (define x (make-basic-constraint '(foo bar) (lambda (f1 f2) (display f1)(display " ")(display f2))))


