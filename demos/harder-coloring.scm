(define *use-network-visualizer* #f)

(declare-form-type 'node (list 'color))
(declare-form 'colors 'basic)
(set-property 'colors 'value '(red blue green))


(declare-form 'A 'node)
(declare-form 'B 'node)
(declare-form 'C 'node)
(declare-form 'D 'node)
(declare-form 'E 'node)
(declare-form 'F 'node)
(declare-form 'G 'node)
(declare-form 'H 'node)
(declare-form 'I 'node)
(declare-form 'J 'node)

(define (Node-constraint x y)
  (if (not (eq? (get-property x 'color) (get-property y 'color)))
    1.0
    0.0))

(define (Node-hint x y)
  (let ((x-color (get-property x 'color))
        (y-color (get-property y 'color)))
    (if (null? x-color)
      (make-binding-list
        (make-binding x (list 'color (car (get-value 'colors)))))
      (cond
        ((and (eq? x-color y-color) (> (random 5) 2))
         (make-binding-list))
        (else
          (make-binding-list
            (random-choice
              (map (lambda (color)
                     (make-binding y (list 'color color)))
                   (filter (lambda (z) (not (eq? z x-color))) (get-value 'colors))))))))))

;;
;;Peterson graph, uh, this is hard to do in ascii art
;;           A 
;;       /   |  \
;;     /     F    \
;;    B--G all H-- E
;;    \            /
;;     \   I  J   /
;;      \ /    \ /
;;       C-----D

(define AB (make-basic-constraint '(A B) Node-constraint Node-hint))
(define BC (make-basic-constraint '(B C) Node-constraint Node-hint))
(define CD (make-basic-constraint '(C D) Node-constraint Node-hint))
(define DE (make-basic-constraint '(D E) Node-constraint Node-hint))
(define EA (make-basic-constraint '(E A) Node-constraint Node-hint))
(define BG (make-basic-constraint '(B G) Node-constraint Node-hint))
(define AF (make-basic-constraint '(A F) Node-constraint Node-hint))
(define HE (make-basic-constraint '(H E) Node-constraint Node-hint))
(define DJ (make-basic-constraint '(D J) Node-constraint Node-hint))
(define IC (make-basic-constraint '(I C) Node-constraint Node-hint))
(define GH (make-basic-constraint '(G H) Node-constraint Node-hint))
(define GJ (make-basic-constraint '(G J) Node-constraint Node-hint))
(define FI (make-basic-constraint '(F I) Node-constraint Node-hint))
(define FJ (make-basic-constraint '(F J) Node-constraint Node-hint))
(define IH (make-basic-constraint '(I H) Node-constraint Node-hint))


(define (coloring-constraint . edges)
  (let ((const (map apply edges)))
    (/ (apply + const) (length const))))

(define all-colored
  (make-compound-constraint
    (list AB BC CD DE EA BG AF HE DJ IC GH GJ FI FJ IH)
    coloring-constraint))

(basic-annealing-solver '(A B C D E F G H I J)
                        (list all-colored) 1000)

