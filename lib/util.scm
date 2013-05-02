(define (listify nodes)
  (cond
    ((list? nodes) nodes)
    (else (list nodes))))

(define (pretty-print nodes)
  (define (print node depth)
    (display node)(write-string " ")(display depth)(newline)i
    (for-each
      (lambda (node)
        (print node (+ 1 depth)))
      '()))
  (for-each (lambda (node) (print node 0)) nodes))
