;; コルーチン
;; 協調型

(define %cc% 'EMPTY-COROUTINE)

;;;; 外部から呼び出す関数

;; コルーチンの生成
(define (make-coroutine f . args)
  (cons 1           ; stat
        (lambda ()  ; procedure or continuation
          (apply f args)
          (term-coroutine!))))

;; コルーチンが生きてるか？
(define (coroutine-alive? co)
  (car co))

;; コルーチン起動
(define (wake-coroutine! co)
  (and (coroutine-alive? co)
       (begin
         (dec! (car co))  ; decrement wait count
         (when (<= (car co) 0)  ; time to wake up
           (let ((prev-cc %cc%))
             (receive (c n) (call/cc
                             (lambda (cc)
                               (set! %cc% cc)
                               ((cdr co))))  ; call coroutine
               (set! %cc% prev-cc)
               (set-car! co n)
               (set-cdr! co c))))
         #t)))

;;;; コルーチン内部から呼び出す関数

;; 自殺する
(define (term-coroutine!)
  (%cc% #f #f))

;; 一時中断
(define (yield . params)
  (let ((n (cond ((null? params) 1)
                 (else (car params)))))
    (call/cc
     (lambda (cc)
       (%cc% cc n)))))
