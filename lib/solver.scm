(define *solver-debug* #t)

;; pretty print debug
(define (ppd . x) (if (or *debug* *solver-debug*) (apply pp x)))
;; pretty print debug short (no newline)
(define (ppds . x) (if (or *debug* *solver-debug*) (apply display x)))

;; Iteratively apply constraints to the objects in the world. The action taken
;; when a constraint fails is implementation/mode specific. Rely on constraint
;; propagation to actually propagate changes through the network.  Backtracking,
;; if needed, will occur at the constraint level.

;; TODO: document me
(define (iteratively-score-hints list-of-hints scoring-function)
  ;; forms-to-hints is an assoc list mapping a form to an list of hints
  (ppds "list-of-hints is ") (ppd list-of-hints)
  (map (lambda (form-bindings-pair)
    (ppds "form -> bindings assoc list is ") (ppd form-bindings-pair)
    (let ( (form (car form-bindings-pair)) (bindings (cdr form-bindings-pair)) )
        (ppds "form -> binding is") (ppds form-bindings-pair)
        (ppds "examining form: ") (ppd form)
        (apply-bindings form bindings)
        (display "bound form: ") (display form) (pp-form form) (newline)
        (scoring-function)o
      )
    ) list-of-hints))

;; given a single objective, returns an list of leaf constraints (which may
;; include the objective constraint, if it is a leaf)
;; TODO: convert to a set so we don't have duplicate leafs included
(define (get-constraint-leaves objective-constraint) 
  (if (objective-constraint 'leaf?)
      objective-constraint
      (map get-constraint-leaves (objective-constraint 'children))))

(define (get-hints target-constraint)
  (target-constraint 'hint))

(define (join-lists list-of-lists)
  (fold-right (lambda (a b) (append a b)) '() list-of-lists))

;; iterate recursively over the objective constraints, descending into their
;; children (not implemented yet) and calling the hint-iterator on each of them
;; with the passed scoring-function.
(define (iterative-solver forms objective-constraints scoring-function)
  (let* ( (root-bindings (map capture-bindings forms))
          (restore-root-bindings (lambda () (apply-bindings forms root-bindings)))
          (all-constraint-leaves (car (map get-constraint-leaves objective-constraints)))
          (all-hints (join-lists (map get-hints all-constraint-leaves))) )

    (display "all-constraint-leaves ") (pp all-constraint-leaves)
    (display "all hints") (pp all-hints)
    (display "all susbsets of hints") (pp (non-empty-subsets all-hints))

    ;; generate the set of all possible subsets of hints, then compute their scores
    (let* ( (all-hints-subsets (non-empty-subsets all-hints)) 
            (accumulated-hint-scores (map (lambda (hint-subset)
                                            (restore-root-bindings)
                                            (iteratively-score-hints hint-subset scoring-function)
                                            ) all-hints-subsets)) )
      (restore-root-bindings)
      (pp accumulated-hint-scores)
  )))

(define (simple-scoring-func objective-constraints objective-constraint-weights )
  (lambda () 
    (let* ( (scores (map (lambda (x) (x)) objective-constraints))
            (weights-and-scores  (zip scores objective-constraint-weights)) )
      (pp weights-and-scores)
          (+ (map (lambda (x) (* (car x) (cdr x))) weights-and-scores))
      )))

;; wrapper to create a solver with a basic scoring function
(define (basic-iterative-solver forms objective-constraints)
  (iterative-solver forms objective-constraints 
                    (simple-scoring-func objective-constraints (make-list (length objective-constraints) 1))))

(define (weighted-iterative-solver forms objective-constraint objective-constraint-weights)
  (iterative-solver forms objective-constraints 
                    (simple-scoring-func objective-constraints objective-constraint-weights)))

;; we will have two initial solver implementations

;; hill-climber - tries to maximize a weighted sum of objective-constraint
;; satisfaction ratings by examining all of the various constraint hints
;; (perhaps using simulated annealing)

;; absolute-solver - if it is discovered that all objective-constraints have
;; been satisfied (are rated 1.0), terminate immediately. if it is discovered
;; that one constraint cannot be satisfied (is rated < 1.0), give up
;; immediately.
