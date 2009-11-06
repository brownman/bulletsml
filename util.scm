
(define-constant PI 3.1415926535)

(define-macro (define-once name . body)
  `(unless (global-variable-bound? (current-module) ',name)
     (define ,name ,@body)))

; not equal
(define (/= a b) (not (= a b)))

; length=1?
(define (single? ls)
  (and (not (null? ls))
       (null? (cdr ls))))

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

; gensym した結果をシンボルに割り当て
(define-macro (w/uniq names . body)
  (if (pair? names)
      `(let (map (lambda (n) (list n '(gensym)))
                 names)
         ,@body)
    `(let1 ,names (gensym)
       ,@body)))

; if に成功したらバインド
(define-macro (iflet var val then . rest)
  (w/uniq g
    `(let1 ,g ,val
       (if ,g
           (let1 ,var ,g
             ,then)
         ,@rest))))

; 値による case
(define-macro (vcase val . rest)
  (w/uniq g
    `(let1 ,g ,val
       (cond ,@(map (lambda (exp)
                      (if (eq? (car exp) 'else)
                          exp
                        `((= ,(car exp) ,g) ,@(cdr exp))))
                    rest)))))

;; リストlsから要素xを取り除く
;; xは１個しかないと仮定
(define (remove-from-list! x ls)
  (let recur ((cur ls)
              (pre #f))
    (cond ((null? cur) ls)
          ((eq? (car cur) x)
           (if pre
               (begin
                 (set-cdr! pre (cdr cur))
                 ls)
             (cdr ls)))
          (else (recur (cdr cur) cur)))))

;; 値の範囲制限
(define (clamp val min max)
  (cond ((< val min) min)
        ((> val max) max)
        (else val)))

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

;; 整数へ変換
(define (to-int x)
  (inexact->exact (floor x)))
