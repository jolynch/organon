
;; Iteratively apply constraints to the objects in the world. The action taken
;; when a constraint fails is implementation/mode specific. Rely on con-
;; straint propagation to actually propagate changes through the network.
;; Backtracking, if needed, will occur at the constraint level.

(define (iterative-solver forms objective-constraints)
  (let ( (root-bindings (map capture-bindings forms)) )
    ;;(pp "root bindings are ") (display root-bindings)
    (for-each (lambda (objective-constraint)
      ;;(pp "objective constraint ") (pp (eval objective-constraint user-initial-environment))
      (let ( (forms-to-hints (apply (eval objective-constraint user-initial-environment) (list 'hint))) )
        ;;(pp "objective hints are ") (display forms-to-hints)
        (for-each (lambda (form)
          ;;(pp "examining form") (pp form) (pp (cdr (assq form forms-to-hints)))
          (apply-bindings form (cdr (assq form forms-to-hints)))
          (display "form : ") (display form) (pp-form form) (newline)
          ) (map car forms-to-hints) )
        )) objective-constraints)
    ))

;; we will have two initial solver implementations

;; hill-climber - tries to maximize a weighted sum of objective-constraint
;; satisfaction ratings by examining all of the various constraint hints
;; (perhaps using simulated annealing)
(define iterative-solver )

;; absolute-solver - if it is discovered that all objective-constraints have
;; been satisfied (are rated 1.0), terminate immediately. if it is discovered
;; that one constraint cannot be satisfied (is rated < 1.0), give up
;; immediately.
