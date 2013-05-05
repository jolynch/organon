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

;; (load "util")
(define hostname "18.96.7.2")
(define port-number 1337)

(define clear-screen-marker "(CLS)")
(define connection '())

(define (frame->packet frame)
  (write-to-string frame))
;; "xyz(q1,q2,q3,q4)")

(define (vertices->packet vertex-list)
  (write-to-string vertex-list))
;;  "(v1,v2,v3)(v4,v5,v6")

(define (form->packet form)
  ;; write out the frame, then the vertex lists
  (assert (lambda () is-type? form '3D-form))

  (let* ( (frame (cond ( (is-type? form 'star) 
                         (make-frame (get-property form 'center) (make-identity-quaternion)))
                       (else (get-property form 'frame)) ))
          (vertex-list (get-property form 'vertices))
          (packet (cond ( (is-type? form 'cylinder-form)
                          (write-to-string (list (list "cylinder") 
                                                 (list (get-property form 'radius)
                                                       (get-property form 'length)))) )
                        ( (is-type? form 'sphere-form)
                          (write-to-string (list (list "sphere") 
                                                 (list (get-property form 'radius)))) )
                        ( (is-type? form 'box-form)
                          (write-to-string (list (list "box")
                                                 (list (get-property form 'height) 
                                                       (get-property form 'width)
                                                       (get-property form 'length)))) )
                        ( (is-type? form 'star)
                          (write-to-string (list (list "sphere")
                                                 (list (get-property form 'radius)))) )
                        (else (vertices->packet vertex-list)) )) )
    (string-append "(" (frame->packet frame) packet  ")")
    ))

(define (forms->packet forms)
  (assert (lambda () (not (null? connection))))
  (string-append clear-screen-marker (fold-right string-append "" (map form->packet forms))))

(define (write-clear-screen)
  (write-line clear-screen-marker connection))

(define (write-forms forms)
  (write-line (forms->packet forms) connection))

(define (close-connection)
  (close-port connection))

(define (make-connection)
  (set! connection (open-tcp-stream-socket hostname port)))
