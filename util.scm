
(define-constant PI 3.1415926535)

; anaphoric-if
(define-macro (aif test-form then-form . else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-form)))

; anaphoric-when
(define-macro (awhen test-form . body)
  `(aif ,test-form
        (begin ,@body)))

; anaphoric-and
(define-macro (aand . args)
  (cond ((null? args) #t)
        ((null? (cdr args)) (car args))
        (else `(aif ,(car args)
                    (aand ,@(cdr args))
                    #f))))

;; 度からラジアンへ
(define (deg->rad deg) (* deg (/ PI 180)))
;; ラジアンから度へ
(define (rad->deg rad) (* rad (/ 180 PI)))

;; atan2
(define (atan2 y x)
  (define (calc dx dy)
    (if (>= dx dy)
        (atan (/ dy dx))
      (- (/ PI 2)
         (atan (/ dx dy)))))
  (if (>= x 0)
      (if (>= y 0)
          (calc x y)
        (- (calc x (- y))))
    (if (>= y 0)
        (- PI (calc (- x) y))
      (- (calc (- x) (- y)) PI))))

(define (to-int x)
  (inexact->exact (floor x)))
