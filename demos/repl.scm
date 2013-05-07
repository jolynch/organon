(define demos
  (list
    (cons 'ladder "ladder.scm")
    (cons 'ladder-annealing "ladder-annealing.scm")
    (cons 'big-bang "big-bang.scm")
    (cons 'laffer "laffer.scm")
    (cons 'coloring "node-coloring.scm")
    (cons 'hard-coloring "harder-coloring.scm")))

(define (display-demos)
  (pp demos))

(define (usage)
  (display "Usage:\n")
  (display "  demos ==> list available demos\n")
  (display "  run-demo ==> select from demos to run\n")
  (display "  help ==> show this help screen\n"))

(define (run-demo)
  (display "Available demos, type one in to run it:\n")
  (for-each
    (lambda (demo)
      (write-string "  - ")(display (car demo))(newline))
    demos)
  (newline)
  
  (display "Which demo? >> ")
  (let* ((input (read))
         (demo (assoc input demos)))
    (display demo)
    (if (not (eq? demo #f))
      (load (cdr demo)))))

(define (run-demos)
  (display "> ")
  (let ((input (read)))
    (cond
      ((eq? input 'demos)
       (display-demos))
      ((eq? input 'help)
       (usage))
      ((eq? input 'run-demo)
       (run-demo)))
    (if (not (or (eq? input 'quit) (eq? input 'q)))
    (run-demos))))

(define (go)
  (run-demos))

(newline)
(run-demos)
