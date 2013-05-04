;; reference
;; outputs: http://web.mit.edu/scheme_v9.0.1/doc/mit-scheme-ref/Output-Procedures.html
;; ports: http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Ports.html
;; tcp: http://web.mit.edu/scheme_v9.0.1/doc/mit-scheme-ref/TCP-Sockets.html

;; Call this to start:
;; > (make-connection)
;;
;; When you want to render a scene, call: 
;; > (write-forms *your-forms-here*)
;; This will clear the screen and redraw all of the forms
;; 
;; > (close-connection) 
;; When you're done

(load "util")

(define clear-screen-marker "(CLS)")
(define port-number 1337)
(define connection '())

(define (frame->packet frame)
  frame) 
;; "xyz(q1,q2,q3,q4)")

(define (vertices->packet vertex-list)
  vertex-list)
;;  "(v1,v2,v3)(v4,v5,v6")

(define (form->packet form)
  ;; write out the frame, then the vertex lists
  (assert (lambda () is-type? form '3D-form))
  (let ( (frame (get-property form 'frame)) (vertex-list (get-property form 'vertices)) )
      (string-append "((" (frame->packet frame) ")(" (vertices->packet vertex-list)")")
  ))

(define (forms->packet forms)
  (assert (lambda () (not (null? connection))))
  (string-append (fold-right string-append "" (map form->packet forms)) clear-screen-marker))

(define (write-clear-screen)
  (write-line clear-screen-marker connection))

(define (write-forms forms)
  (write-line (forms->packet forms) connection))

(define (close-connection)
  (close-port connection))

(define (make-connection)
  (set! connection (open-tcp-stream-socket "localhost" port-number)))

;; (pp "making connection")
;; (make-connection)
;; (pp "writing port")
;; (write-forms (list 3220 32820 3282))
;; (pp "closing port")
;; (close-connection)
