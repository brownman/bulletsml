;; BulletSML

(use srfi-1)   ; filter
(use srfi-27)  ; random-real
(use srfi-13)  ; for string-concatenate

(require "./co")
(require "./util")

(define rand random-real)

;; XY���W����p�x��Ԃ�
;; ���W�A������Ȃ��ēx
;; ��(Y-)���O�Ƃ��Ď��v���
(define (get-angle dx dy)
  (rad->deg (atan2 dx (- dy))))

;; �����Ƒ������瑬�x���v�Z
;; �����͓x
;; �������0�Ŏ��v���
(define (calc-velocity deg speed)
  (let ((rad (deg->rad deg)))
    (values (* speed    (sin rad))
            (* speed (- (cos rad))))))

;; �p�x�̐��K�� (-180�`+180�Ɏ��߂�)
(define (degree deg)
  (cond ((> deg 180)
         (- (fmod (+ deg 180) 360) 180))
        ((< deg -180)
         (+ (fmod (- deg 180) 360) 180))
        (else deg)))

;; ������Ƃ��Č���
(define (cat . args)
  (string-concatenate (map x->string args)))

;; �c���[�̊e�m�[�h��ϊ�����
(define (replace-node s f)
  (cond ((pair? s)
         (cons (replace-node (car s) f)
               (replace-node (cdr s) f)))
        (else (f s))))

;; �ϐ�($...)��u��������
(define (replace-variable s)
  (define (conv x)
    (cond ((#/^\$(\d+)$/ (symbol->string x)) =>
           (lambda (m)
             `(ref $args ,(- (read-from-string (m 1)) 1))))
          (else x)))
  (replace-node s
                (lambda (x)
                  (cond ((symbol? x) (conv x))
                        (else x)))))

;;==========================
;; BulletML �^�O����

(define (action-name sym) (string->symbol (cat "%action-" sym)))
(define (bullet-name sym) (string->symbol (cat "%bullet-" sym)))
(define (fire-name sym)   (string->symbol (cat "%fire-" sym)))

;; <bulletml> �^�O
(define-macro (bulletml . params)
  (let ((body (get-elem-body params)))
    (let ((actions (listup-nodes body 'action))
          (bullets (listup-nodes body 'bullet))
          (fires (listup-nodes body 'fire)))
      `(letrec (,@(map (lambda (action)
                         `(,(action-name (get-property (cdr action) :label))
                           (lambda (self . $args)
                             ,action)))
                       actions)
                ,@(map (lambda (self)
                         (let ((body (get-elem-body (cdr self))))
                           (let ((speed (or (aand (find-node body 'speed)
                                                  (cadr it))
                                            1))
                                 (actions (listup-nodes body
                                                        (lambda (tag)
                                                          (member tag '(action actionRef))))))
                             `(,(bullet-name (get-property (cdr self) :label))
                               (list ,speed
                                     (lambda (self . $args)
                                       ,@actions))))))
                       bullets)
                ,@(map (lambda (fire)
                         `(,(fire-name (get-property (cdr fire) :label))
                           (lambda (self . $args)
                             ,fire)))
                       fires))
               ,(action-name 'top)))))

;; <action> �^�O
(define-macro (action . params)
  (let ((body (get-elem-body params)))
    `(begin ,@body)))

;; <actionRef> �^�O
(define-macro (actionRef . params)
  (let ((body (get-elem-body params)))
    (let ((label (action-name (get-property params :label)))
          (args (map (lambda (x)
                       (replace-variable (cadr x)))
                     (listup-nodes body 'param))))
      `(,label self ,@args))))

;; <bullet> �^�O
(define-macro (bullet . params)
  (let ((body (get-elem-body params)))
    (let ((speed (or (find-node body 'speed)
                     1))
          (dir (or (aand (find-node body 'direction)
                         (cadr it))
                   0))
          (actions (listup-nodes body
                                 (lambda (tag)
                                   (member tag '(action actionRef))))))
      `(begin ,@actions))))

;; <repeat> �^�O
(define-macro (repeat . params)
  (let ((root `(repeat ,@params)))
    (let ((times (or (aand (find-node params 'times)
                           (replace-variable (cadr it)))
                     1))
          (actions (listup-nodes params 'action))
          (i (gensym)))
      `(dotimes (,i ,times)
         ,@actions))))

;; <fire> �^�O
(define-macro (fire . params)
  (let* ((body (get-elem-body params))
         (dir (find-node body 'direction)))
    (cond ((find-node body 'bullet) =>
           (lambda (node)
             (let ((speed (or (find-node body 'speed)
                              1)))
               `(fire-bullet self (lambda (self) ,node) ,dir ,speed))))
          ((find-node body 'bulletRef) =>
           (lambda (node)
             (let ((body (get-elem-body (cdr node))))
               (let ((bullet-info (bullet-name (get-property (cdr node) :label)))
                     (args (map (lambda (x)
                                  (replace-variable (cadr x)))
                                (listup-nodes body 'param))))
                 `(fire-bullet self
                               (lambda (self)
                                 ((cadr ,bullet-info) self ,@args))
                               ,dir
                               (car ,bullet-info))))))
          (else (error "fire: no bullet node")))))

;; <fireRef> �^�O
(define-macro (fireRef . params)
  (let ((body (get-elem-body params)))
    (let ((label (fire-name (get-property params :label)))
          (args (map (lambda (x)
                       (replace-variable (cadr x)))
                     (listup-nodes body 'param))))
      `(,label self ,@args))))

;; <wait> �^�O
(define-macro (wait . params)
  (let ((n (replace-variable (car params))))
    `(yield ,n)))

;; <direction> �^�O
(define-macro (direction . params)
  (let ((type (cond ((get-property params :type))
                    (else 'aim)))
        (val (replace-variable (car (get-elem-body params)))))
    (case type
      ((sequence) `(direction-sequence self ,val))
      ((absolute) `(direction-absolute self ,val))
      ((relative) `(direction-relative self ,val))
      (else `(direction-aim self ,val)))))

;; <speed> �^�O
(define-macro (speed . params)
  (let ((val (car (get-elem-body params))))
    val))

(define (direction-sequence self param)
  (emitter-direction-sequence self param))

(define (direction-absolute self param)
  (emitter-direction-absolute self param))

(define (direction-aim self param)
  (emitter-direction-aim self param))

(define (direction-relative self param)
  (print "**** direction-relative not implemented yet")
  (emitter-direction-absolute self param))

;; <vanish> �^�O
(define-macro (vanish)
  `(vanish-bullet self))

;; <term> �^�O
(define-macro (term . params)
  (replace-variable (car params)))

;; <changeDirection> �^�O
(define-macro (changeDirection . params)
  (let ((dir (find-node params 'direction))
        (term (find-node params 'term)))
    `(change-direction self
                       ,dir
                       ,term)))

;; <changeSpeed> �^�O
(define-macro (changeSpeed . params)
  (let ((speed (find-node params 'speed))
        (term (find-node params 'term)))
    `(change-speed self
                       ,speed
                       ,term)))

;; <accel> �^�O
(define-macro (accel . params)
  (let ((body (get-elem-body params)))
    (let ((horz (or (aand (find-node body 'horizontal)
                          (replace-variable (car (get-elem-body (cdr it)))))
                    0))
          (vert (or (aand (find-node body 'vertical)
                          (replace-variable (car (get-elem-body (cdr it)))))
                    0))
          (term (or (aand (find-node body 'term)
                          (replace-variable (car (get-elem-body (cdr it)))))
                    1)))
      `(bullet-accel-set! self
                          ,horz
                          ,vert
                          ,term))))


;;==========================
;; SML �֘A

;; �^�O��񋓂���
(define (listup-nodes nodes tag)
  (if (procedure? tag)
      (filter (lambda (node) (tag (car node)))
              nodes)
    (filter (lambda (node) (eq? (car node) tag))
            nodes)))

;; �^�O�Ńm�[�h��T��
(define (find-node bml tag)
  (if (procedure? tag)
      (let recur ((ls bml))
        (cond ((null? ls) #f)
              ((tag (car ls)) (car ls))
              (else
               (recur (cdr ls)))))
    (let recur ((ls bml))
      (cond ((null? ls) #f)
            ((eq? (caar ls) tag) (car ls))
            (else
             (recur (cdr ls)))))))

;; �v���p�e�B�̎擾
(define (get-property belem key)
  (let1 r (assoc key (keylist->alist belem))
    (if r
        (cdr r)
      #f)))

;; �G�������g�̃L�[���[�h���X�g��alist�ɕϊ����Ď擾
(define (keylist->alist belem)
  (let recur ((ls belem)
              (acc '()))
    (cond ((null? ls) acc)
          ((keyword? (car ls))
           (recur (cddr ls)
                  (cons (cons (car ls)
                              (cadr ls))
                        acc)))
          (else acc))))

;; �G�������g�̖{�̎擾
(define (get-elem-body belem)
  (let recur ((ls belem))
    (cond ((null? ls) '())
          ((keyword? (car ls)) (recur (cddr ls)))
          (else ls))))


;;==========================
;; bullet

(define (make-bullet x y dir speed proc)
  (let ((changeDirTerm 0)
        (targetDir #f)
        (changeSpdTerm 0)
        (targetSpd #f))
    (receive (vx vy) (calc-velocity dir speed)
      (let ((self
             (list->vector
              `(()  ; co
                ,x
                ,y
                ,dir
                ,speed
                ,changeDirTerm
                ,targetDir
                ,changeSpdTerm
                ,targetSpd
                ,vx
                ,vy))))
        (let ((co (make-coroutine proc self)))
          (vector-set! self 0 co))
        self))))

(define (bullet-x self)
  (vector-ref self 1))

(define (bullet-y self)
  (vector-ref self 2))

(define (update-bullet self)
  (let (
        ; �R���[�`���N��
        (wake-coro (lambda ()
                     (wake-coroutine! (vector-ref self 0))))
        ; �p�x�X�V
        (update-dir (lambda ()
                      (if (> (vector-ref self 5) 0)
                          (begin
                            (vector-set! self 3
                                         (+ (vector-ref self 3)
                                            (/. (degree (- (vector-ref self 6)
                                                           (vector-ref self 3)))
                                                (vector-ref self 5))))
                            (dec! (vector-ref self 5))
                            #t)
                        #f)))
        ; �����X�V
        (update-spd (lambda ()
                      (if (> (vector-ref self 7) 0)
                          (begin
                            (vector-set! self 4
                                         (+ (vector-ref self 4)
                                            (/ (- (vector-ref self 8)
                                                  (vector-ref self 4))
                                               (vector-ref self 7))))
                            (dec! (vector-ref self 7))
                            #t)
                        #f)))
        ; ���x���X�V����
        (update-velocity (lambda ()
                           (receive (vx vy) (calc-velocity (vector-ref self 3) (vector-ref self 4))
                             (vector-set! self  9 vx)
                             (vector-set! self 10 vy))))
        ; �ړ�
        (move (lambda ()
                (let ((vx (vector-ref self 9))
                      (vy (vector-ref self 10)))
                  (vector-set! self 1 (+ (bullet-x self) vx))
                  (vector-set! self 2 (+ (bullet-y self) vy))))))
    
    (wake-coro)
    (when (or (update-dir)
              (update-spd))
      (update-velocity))
    (move)))

(define (vanish-bullet self)
  (vector-set! self 1 -100))  ; ��ʊO�ɔ�΂��ď����Ă��炤

(define (change-direction self dir term)
  (vector-set! self 5 term)
  (vector-set! self 6 dir))

(define (change-speed self dir term)
  (vector-set! self 7 term)
  (vector-set! self 8 dir))

(define (bullet-accel-set! self horz vert term)
  (print "**** bullet-accel-set! not implemented yet"))

(define (render-bullet self sprite)
  (blit sprite
        (to-int (- (bullet-x self) 4))
        (to-int (- (bullet-y self) 4))))

;;==========================
;; emitter

(define (make-emitter game bml-proc)
  (let ((x 150)
        (y 50)
        (dir 0))
    (let ((self
           (list->vector
            `(()  ; co
              ,x
              ,y
              ,dir
              ,game))))
      (let ((co (make-coroutine bml-proc self)))
        (vector-set! self 0 co))
      self)))

(define (emitter-x self) (vector-ref self 1))
(define (emitter-y self) (vector-ref self 2))

(define (emitter-game self)
  (vector-ref self 4))

(define (emitter-fire-degree-set! self deg)
  (let ((a (degree deg)))
    (vector-set! self 3 a)
    a))

(define (emitter-direction-sequence self ofs)
  (+ (vector-ref self 3)
     ofs))

(define (emitter-direction-aim self ofs)
  (let* ((player (game-player (emitter-game self)))
         (ang (get-angle (- (player-x player) (emitter-x self))
                         (- (player-y player) (emitter-y self)))))
    (+ ang ofs)))

(define (emitter-direction-absolute self deg)
  deg)

(define (update-emitter self)
  (let ((co (vector-ref self 0)))
    (wake-coroutine! co)))

(define (fire-bullet self bproc dir speed)
  ;(print #`"** fire ,bproc ,dir ,speed")
  (emitter-fire-degree-set! self dir)
  (add-bullet (emitter-game self)
              (make-bullet (emitter-x self) (emitter-y self) dir speed bproc)))
