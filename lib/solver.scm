(define *solver-debug* #f)

;; pretty print debug
(define (ppd . x) (if (or *debug* *solver-debug*) (apply pp x)))
;; pretty print debug short (no newline)
(define (ppds . x) (if (or *debug* *solver-debug*) (apply display x)))

;; Iteratively apply constraints to the objects in the world. The action taken
;; when a constraint fails is implementation/mode specific. Rely on constraint
;; propagation to actually propagate changes through the network.  Backtracking,
;; if needed, will occur at the constraint level.

;; TODO: document me
(define (iteratively-score-hints list-of-hints scoring-function visualizer-function)
  ;; forms-to-hints is an assoc list mapping a form to an list of hints
  (ppds "list-of-hints is ") (ppd list-of-hints)
  (map (lambda (form-bindings-pair)
    (ppds "form -> bindings assoc list is ") (ppd form-bindings-pair)
    (let ( (form (car form-bindings-pair)) (bindings (car (cdr form-bindings-pair))) )
        (ppds "binding are") (ppds bindings)
        (ppds "examining form: ") (ppd form)
        (apply-bindings form bindings)
        (display "bound form: ") (display form) (pp-form form) (newline)
        (visualizer-function)
        (scoring-function)
      )
    ) list-of-hints))

;; given a single objective, returns an list of leaf constraints (which may
;; include the objective constraint, if it is a leaf)
;; TODO: convert to a set so we don't have duplicate leafs included
(define (get-constraint-leaves objective-constraint) 
  (if (objective-constraint 'leaf?)
      (list objective-constraint)
      (map (lambda (x) (car (get-constraint-leaves x))) (objective-constraint 'children))))

(define (get-hints target-constraint)
  (target-constraint 'hint))

(define (join-lists list-of-lists)
  (fold-right (lambda (a b) (append a b)) '() list-of-lists))

(define (network-visualizer all-forms)
  (write-forms all-forms))

;; iterate recursively over the objective constraints, descending into their
;; children (not implemented yet) and calling the hint-iterator on each of them
;; with the passed scoring-function.
(define (iterative-solver forms objective-constraints scoring-function)
  (let* ( (root-bindings (map capture-bindings forms))
          (restore-root-bindings (lambda () (apply-bindings forms root-bindings)))
          (all-constraint-leaves (car (map get-constraint-leaves objective-constraints)))
          (all-hints (join-lists (map get-hints all-constraint-leaves))))

    (display "all-constraint-leaves ") (pp all-constraint-leaves)
    (display "all hints") (pp all-hints)
    (display "all susbsets of hints") (pp (non-empty-subsets all-hints))

    ;; generate the set of all possible subsets of hints, then compute their scores
    (let* ((all-hints-subsets (non-empty-subsets all-hints))

            (accumulated-hint-scores (map (lambda (hint-subset)
                                            (restore-root-bindings)
                                            (iteratively-score-hints hint-subset scoring-function 
                                              (lambda () (network-visualizer forms)))
                                            ) all-hints-subsets)) )
      (restore-root-bindings)
      (pp accumulated-hint-scores)
  )))

(define (anneal-choose hints iteration prob)
  (let ((chosen-hints
          (let loop ((result '())
                     (remaining hints))
            (if (null? remaining) result
              (let ((value (car remaining)))
                (if (< (random 1.0) prob)
                  (loop (cons value result) (cdr remaining))
                  (loop result (cdr remaining))))))))
    (better-bindings chosen-hints)))

;; Takes bindings of the form
;; (form property value) ...
(define (apply-better-bindings chosen-bindings)
  (for-each
    (lambda (binding)
      (set-property (first binding) (second binding) (third binding)))
    chosen-bindings))

(define (show-state forms)
  (for-each (lambda (form)
              (display "Form: ")(write form)(newline)
             (display "Bindings:")(pp-form form)(newline)
              (newline)) forms))

(define (annealing-solver o-forms objectives scoring temperature iterations)
  (let ((best-binding (map capture-bindings o-forms))
        (best-value (scoring)))
    (define (solve forms objective-constraints scoring-function temp iter)
      (let* ( (root-bindings (map capture-bindings forms))
             (restore-root-bindings (lambda () (apply-bindings forms root-bindings)))
             (all-constraint-leaves (car (listify (map get-constraint-leaves objective-constraints))))
             (all-hints (join-lists (map get-hints all-constraint-leaves)))
             (converted-hints (filter (lambda (result) (not (null? result)))
                                      (map
                                        (lambda (hint)
                                          (better-bindings (list hint)))
                                        all-hints)))
             (all-bindings (remove-dups converted-hints)))

        ;; generate the set of all possible subsets of hints, then compute their scores
        (let ((chosen-bindings (anneal-choose all-hints 0 temp)))
          (apply-better-bindings chosen-bindings)
          (network-visualizer forms)
          (let ((score (scoring-function)))
            (if (> score best-value)
              (begin
                (set! best-binding (map capture-bindings forms))
                (set! best-value score)))
            (cond
              ((< iter 0)
               (pp "Exceeded maximum iterations, best answer is:")
               (apply-bindings forms best-binding)
               (show-state forms)
               (pp "Got as good as:")(display score))
              ((> score .98)
               (pp "Found solution state:")
               (show-state forms)
               (pp "Exiting\n ..."))
              (else
                (display "Trying again ")(write score)(display " is not good enough!")(newline)
                (solve forms objective-constraints scoring-function (* .98 temp) (- iter 1))))))))
      (solve o-forms objectives scoring temperature iterations)))



(define (simple-scoring-func objective-constraints objective-constraint-weights )
  (lambda () 
    (let* ( (scores (map (lambda (x) (x)) objective-constraints))
            (weights-and-scores (zip scores objective-constraint-weights)) )
      (apply + (map (lambda (x) (apply * x)) weights-and-scores))
      )))

;; wrapper to create a solver with a basic scoring function
(define (basic-iterative-solver forms objective-constraints)
  (iterative-solver forms objective-constraints 
                    (simple-scoring-func objective-constraints (make-list (length objective-constraints) 1))))

(define (weighted-iterative-solver forms objective-constraint objective-constraint-weights)
  (iterative-solver forms objective-constraints 
                    (simple-scoring-func objective-constraints objective-constraint-weights)))

(define (basic-annealing-solver forms objective-constraints iterations)
  (pp "Initial state:")
  (show-state forms)
  (newline)
  (annealing-solver forms objective-constraints
                    (simple-scoring-func objective-constraints (make-list (length objective-constraints) 1)) 1 iterations))

;; we will have two initial solver implementations

;; hill-climber - tries to maximize a weighted sum of objective-constraint
;; satisfaction ratings by examining all of the various constraint hints
;; (perhaps using simulated annealing)

;; absolute-solver - if it is discovered that all objective-constraints have
;; been satisfied (are rated 1.0), terminate immediately. if it is discovered
;; that one constraint cannot be satisfied (is rated < 1.0), give up
;; immediately.
