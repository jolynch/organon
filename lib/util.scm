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
