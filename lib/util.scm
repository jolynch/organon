(define (listify nodes)
  (cond
    ((list? nodes) nodes)
    (else (list nodes))))

(define (pretty-print nodes)
  (define (print node depth)
    (display node)(write-string " ")(display depth)(newline)
    (for-each
      (lambda (node)
        (print node (+ 1 depth)))
      '()))
  (for-each (lambda (node) (print node 0)) nodes))


;; subsets based on http://pages.cs.wisc.edu/~fischer/cs538.s08/lectures/Lecture13.4up.pdf
(define (subset-extend L E)
 (append L (subset-distrib L E)))

(define (subset-distrib L E)
  (if (null? L)
      ()
      (cons (cons E (car L)) (subset-distrib (cdr L) E))))

(define (subsets L)
  (if (null? L)
      (list ())
      (subset-extend (subsets (cdr L)) (car L))))

(define (non-empty-subsets L)
  (filter (lambda (x) (not (null? x))) (subsets L)))

(define (assert expression)
  (if (not (expression)) (error (string-append "assertion failed for expression: " expression))))


;; Bindings are of the form:
;; ((property . value) (property . value) ...)
;;
;; Binding lists are of the form
;; ((form . binding) (form . binding) ...
;;

(define (make-binding-list . bindings)
  bindings)

(define (make-property-binding property value)
  (cons property value))

(define (make-binding form . property-bindings)
  (cons form
        (list (map (lambda (pb) (make-property-binding (car pb) (cadr pb)))
             property-bindings))))

(define (bindings-for bindings form)
  (assoc-get form bindings))

(define (assoc-get object alist)
  (let ((value (assoc object alist)))
    (cond
      ((eq? value #f) #f)
      ((list? value) (cadr value))
      ((pair? value) (cdr value))
      (else #f))))

