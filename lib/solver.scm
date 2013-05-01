
;; Iteratively apply constraints to the objects in the world. The action taken
;; when a constraint fails is implementation/mode specific. Rely on con-
;; straint propagation to actually propagate changes through the network.
;; Backtracking, if needed, will occur at the constraint level.

;; (const)        -> eval constraints (returns range [0, 1])
;; (const 'hint)  -> returns a list of a list of bindings for (form1, form2, ...) 
;;                  of the form (bindings1, bindings2, ...) where bindings are 
;;                  (property -> value, property -> value).
;; (const forms)  -> ??

(define (iterative-solver forms objective-constraints)
  ;; capture the current bindings of the forms
  (let ( (root-bindings (map capture-bindings forms)) )
    (for-each (lambda (objective-constraint) 
      (let ( (this-objectives-hints (objective-constraints 'hint)) )
        (for-each (lambda (form-to-hint-bindings)
          (let ( (form (car form-to-hint-bindings))
                 (hint-bindings (cdr form-to-hint-bindings)) )
            (apply-bindings form hint-bindings)
            (pp-form form)
           )) this-objectives-hints)
        ) objective-constraints))
))
