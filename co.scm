;; �R���[�`��
;; �����^

(define %cc% 'EMPTY-COROUTINE)

;;;; �O������Ăяo���֐�

;; �R���[�`���̐���
(define (make-coroutine f . args)
  (cons 1           ; stat
        (lambda ()  ; procedure or continuation
          (apply f args)
          (term-coroutine!))))

;; �R���[�`���������Ă邩�H
(define (coroutine-alive? co)
  (car co))

;; �R���[�`���N��
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

;;;; �R���[�`����������Ăяo���֐�

;; ���E����
(define (term-coroutine!)
  (%cc% #f #f))

;; �ꎞ���f
(define (yield . params)
  (let ((n (cond ((null? params) 1)
                 (else (car params)))))
    (call/cc
     (lambda (cc)
       (%cc% cc n)))))
