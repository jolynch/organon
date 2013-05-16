(define *debug* #f)

;; Mapping of forms to dependent constraints
(define *forms* (make-eq-hash-table))
;; Mapping of constrains to dependent constraints
(define *constraints* (make-eq-hash-table))

;; Convenience methods to deal with constraint entity state
(define (dirty? entity)
  (eq? (car (entity-extra entity)) 'dirty))

(define (make-dirty entity)
  (set-entity-extra! entity '(dirty)))

(define (make-clean entity value)
  (set-entity-extra! entity (cons 'clean value)))

(define (get-cached entity)
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
;; TODO: pass list of calling dependencies to detect loops
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
;;
;; (constraint 'eval arg1 arg2 ,,,) -> Returns the result of applying func to
;; the supplied arguments
;;
;; (constraint 'type) -> Returns the type of the constraint, currently just
;; basic and compound are supported
;;
;; (constraint 'force) -> Force an evaluation of the function, disregarding the
;; cached value and dirty status of the node
;;
;; (constraint 'leaf?) -> Tests whether this is a leaf node, used by the solver
;; to select nodes dependent on forms

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
    ((test-args 'leaf?)
     (eq? type 'form))
    ((test-args 'force)
     (apply func operands))
    ((dirty? self)
     (if *debug* (pp "Evaluating constraint"))
     (let ((value (apply func operands)))
       (make-clean self value)
       (propagate self)
       value))
    (else
      (if *debug* (begin (display "Using cached value for ")(write self)(newline)))
      (get-cached self))))

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

