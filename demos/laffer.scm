(define *use-network-visualizer* #f)
(define num-taxpayers 100)
(define exponent 10)

(declare-form 'govt-tax-rate 'basic)
(set-property 'govt-tax-rate 'value 1.00)

(declare-form-type 'taxpayer (list 'max-hours-worked 'hourly-wage 'liberalness))
(for-each (lambda (i) (declare-form (symbol 'taxpayer- i) 'taxpayer)) (range 0 num-taxpayers))

;; each taxpayer earns a random amount between $0/hour and $60/hour
(for-each (lambda (i) 
  (set-property (symbol 'taxpayer- i) 'hourly-wage (* 60 (random 1.0)))
  (set-property (symbol 'taxpayer- i) 'liberalness (+ 1 (random 10.0)))) (range 0 num-taxpayers))

;; each taxpayer has a different linear work function that maps their tax
;; rate to the number of hours they are willing to work
(for-each (lambda (i) 
  (set-property (symbol 'taxpayer- i) 'max-hours-worked (* 40 (+ 1 (random 1.0))))) (range 0 num-taxpayers))



;; constraint returns a value in range [0.0, 1.0] is (revenue at tax-rate /
;; maximum possible revenue)
(define (laffer-constraint tax-rate . taxpayers)
  (/ (apply + (map (lambda (ith-taxpayer)
                     ;; (1 - t^l) * maximum hours = number of hours worked
                     ;; * hourly wages = output of this person
                     ;; * tax rate = taxes collected by the government
                     (* (- 1.0 (expt (get-value tax-rate) (get-property ith-taxpayer 'liberalness)))
                        (get-property ith-taxpayer 'max-hours-worked)
                        (get-property ith-taxpayer 'hourly-wage)
                        (get-value tax-rate)))
                   taxpayers))
     (apply + (map (lambda (ith-taxpayer)
                     ;; Maximum possible output of the economy
                     (* (get-property ith-taxpayer 'max-hours-worked)
                        (get-property ith-taxpayer 'hourly-wage))) taxpayers))))

;; TODO: if the hints allow base-hours worked to change, this version of the
;; constraint will consider the overall gov't revenue as well as the hapiness of
;; the citizens (presumably happiness will decrease as max-hours-worked goes up)

;; four hints: + tax rate, - tax rate, + max-hours-worked, - max-hours-worked
(define (laffer-hint tax-rate . taxypayers)
  ;; Everyone wants lower taxes!
  (make-binding-list (make-binding tax-rate (list 'value (max 0.01 (- (get-value tax-rate) 0.01))))))

(define laffer-curve
  (make-basic-constraint
    (cons 'govt-tax-rate (map (lambda (i) (symbol 'taxpayer- i)) (range 0 num-taxpayers)))
    laffer-constraint
    laffer-hint))

(pp "starting")

;;(iterative-solver (map (lambda (i) (symbol 'taxpayer- i)) (range 0 num-taxpayers)) (list laffer-curve))

;; maximimize govt revenue as a percentage of GDP
(basic-annealing-solver (cons 'govt-tax-rate (map (lambda (i) (symbol 'taxpayer- i)) (range 0 num-taxpayers))) (list laffer-curve) 100)

(display "tax rate settled at ")
(pp (get-value 'govt-tax-rate))
