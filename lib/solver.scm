(define *solver-debug* #t)

;; pretty print debug 
(define (ppd . x) (if (or *debug* *solver-debug*) (apply pp x)))
;; pretty print debug short (no newline)
(define (ppds . x) (if (or *debug* *solver-debug*) (apply display x)))

;; Iteratively apply constraints to the objects in the world. The action taken
;; when a constraint fails is implementation/mode specific. Rely on constraint
;; propagation to actually propagate changes through the network.  Backtracking,
;; if needed, will occur at the constraint level.

;; iterate over a targeted constraint, calling the supplied scoring function
;; after applying the hint's suggested bindings. Return a list of the scores
;; corresponding to the constraint's hints.
(define (hint-iterator target-constraint scoring-function)
  ;; forms-to-hints is an assoc list mapping a form to an list of hints
  (ppd "iterator") (ppd user-initial-environment)
  (let ( (forms-to-hints (apply (eval target-constraint user-initial-environment) (list 'hint))) )
    (ppds "objective hints are ") (ppd forms-to-hints)
    (map (lambda (form-binding-pair)
      (let ( (form (car form-binding-pair)) (binding (cdr form-binding-pair)) )
        (ppds "examining form: ") (ppd form)
        (apply-bindings form binding)
        (display "bound form: ") (display form) (pp-form form) (newline)
        (scoring-function)
      )) forms-to-hints)
  ))

;; iterate recursively over the objective constraints, descending into their
;; children (not implemented yet) and calling the hint-iterator on each of them
;; with the passed scoring-function.
(define (iterative-solver forms objective-constraints scoring-function)
  (let ( (root-bindings (map capture-bindings forms)) )
    ;;(pp "root bindings are ") (display root-bindings)
    (for-each (lambda (objective-constraint)
      (ppds "objective constraint: ") (ppd objective-constraint)
      (let ( (hint-scores (hint-iterator objective-constraint scoring-function)) )
        ;; make some sort of decision...but we don't know how yet, so we'll
        ;; just pretty print!
        (ppd hint-scores)
        (pretty-print objective-constraints)
      )
     ) objective-constraints)
  ))

;; wrapper to create a solver with a really stupid scoring function
(define (basic-iterative-solver forms objective-constraints)
  (iterative-solver forms objective-constraints (lambda () 1.0)))

;; we will have two initial solver implementations

;; hill-climber - tries to maximize a weighted sum of objective-constraint
;; satisfaction ratings by examining all of the various constraint hints
;; (perhaps using simulated annealing)

;; absolute-solver - if it is discovered that all objective-constraints have
;; been satisfied (are rated 1.0), terminate immediately. if it is discovered
;; that one constraint cannot be satisfied (is rated < 1.0), give up
;; immediately.
