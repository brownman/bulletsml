;; コルーチン
;; 協調型

; ファイル内で使用：呼び出したコルーチンからの戻り先の継続を保持
(define %cc% 'EMPTY-COROUTINE)

;;;; 外部から呼び出す関数

;; コルーチンの生成
(define (make-coroutine f . args)
  (let1 proc (lambda ()
               (apply f args)
               (term-coroutine!))
    (cons 1        ; stat or wait counter
          proc)))  ; procedure or continuation

;; コルーチンが生きてるか？
(define (coroutine-alive? co)
  (car co))

;; コルーチン起動
(define (wake-coroutine co)
  (define (decrement-wait-counter co) (dec! (car co)))
  (define (time-to-wake-up co) (<= (car co) 0))
  (define (call-co co) (apply (cdr co) '()))

  (if (coroutine-alive? co)
      (begin
        (decrement-wait-counter co)
        (when (time-to-wake-up co)
          (let ((prev-cc %cc%))
            (receive (next-co wait) (call/cc
                                     (lambda (cc)
                                       (set! %cc% cc)
                                       (call-co co)))
              (set! %cc% prev-cc)
              (set-car! co wait)
              (set-cdr! co next-co))))
        #t)
    #f))

;;;; コルーチン内部から呼び出すための関数

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
