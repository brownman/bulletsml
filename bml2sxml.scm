#!/usr/local/bin/gosh

; BulletML から BulletSML へ変換

(use sxml.ssax)
(use pp)

(define (parse-xml port)
  (car (xmlparser port '())))

(define (parse-xml-string xml-string)
  (parse-xml (open-input-string xml-string)))

(define xmlparser
  (ssax:make-parser
   NEW-LEVEL-SEED (lambda args '())
   FINISH-ELEMENT (lambda (elem-gi attributes namespaces expected-content seed)
                    `(,@expected-content (,(if (pair? elem-gi) (cdr elem-gi) elem-gi) ,@(attrlist->keylist attributes) ,@seed)))
   CHAR-DATA-HANDLER (lambda (text b c)
                       (let1 s (strip text)
                         (if (string=? s "")
                             (if (null? c)
                                 '()
                               c)
                           (if (null? c)
                               (list (str->value s))
                             (error "illegal")))))))

(define (attrlist->keylist attrlist)
  (apply append (map (lambda (e)
                       (list (make-keyword (car e))
                             ;(str->value (cdr e))))
                             (string->symbol (cdr e))))
                     attrlist)))

;; 文字列をトークンに分割し、リストを返す
(define (tokenize str)
  (receive (v m) (cond ((#/^\s*(\d+(\.\d*)?)/ str) => (lambda (m) (values (read-from-string (m 1)) m))) ; 数値
                       ((#/^\s*([+\-*\/])/ str) => (lambda (m) (values (string->symbol (m 1)) m))) ; 演算子
                       ((#/^\s*([()])/ str) => (lambda (m) (values (m 1) m))) ; ()
                       ((#/^\s*($(\d+|\w+))/ str) => (lambda (m) (values (m 1) m))) ; $変数
                       ((#/^\s*$/ str) (values #f #f))
                       (else (raise #`"illegal token: ,str")))
    (if v
        (cons v (tokenize (rxmatch-after m)))
      '())))

;; 中置記法の数式リストをパースして、前置記法にして返す
(define (parse-infix-ops ops sub-parser)
  (lambda (ls)
    (define (merge e1 e2)
      (if e1
          (reverse! (cons e2 e1))
        e2))
    (let recur ((e1 #f)
                (ls ls))
      (receive (e2 ls2) (sub-parser ls)
        (if (null? ls2)
            (values (merge e1 e2) '())
          (if (member (car ls2) ops)
              (recur (list (merge e1 e2) (car ls2)) (cdr ls2))
            (values (merge e1 e2) ls2)))))))

(define (parse-factor ls)
  (if (null? ls)
      (raise "illegal eos")
    (let1 x (car ls)
      (cond ((equal? x "(")
             (receive (v ls2) (parse-exp (cdr ls))
               (if (or (null? ls2) (not (equal? (car ls2) ")")))
                   (raise "no close paren")
                 (values v (cdr ls2)))))
            ((equal? x "$rand") (values '(rand) (cdr ls)))
            ((equal? x "$rank") (values '(rank) (cdr ls)))
            ((number? x) (values x (cdr ls)))
            ((string? x) (values (string->symbol x) (cdr ls)))
            ((eq? x '+) (parse-factor (cdr ls)))
            ((eq? x '-)
             (receive (v ls2) (parse-factor (cdr ls))
               (values `(- ,v) ls2)))
            (else (raise #`"illegal factor ,x (,ls)"))))))

(define parse-term (parse-infix-ops '(* /) parse-factor))

(define parse-exp (parse-infix-ops '(+ -) parse-term))

(define (str->value str)
  (cond ((#/^\d+(\.\d+)?$/ str) ; 数値
         => (lambda (_) (read-from-string str)))
        ((#/^[\w_:]+$/ str) ; 識別子
         => (lambda (_) (string->symbol str)))
        (else
         (receive (v rest) (parse-exp (tokenize str))
           (if (null? rest)
               v
             (raise #`"illegal expression: ,(list str v rest)"))))))

;; 文字列前後の空白の削除
(define (strip str)
  (cond ((#/^\s*(.*?\S+)\s*$/ str) => (lambda (m) (m 1)))
        (else "")))

(define (main args)
  (if (null? (cdr args))
      (pp (parse-xml (standard-input-port)))
    (call-with-input-file (cadr args)
                          (lambda (port)
                            (pp (parse-xml port))))))
