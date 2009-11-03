;; BulletSML

(use srfi-1)   ; filter
(use srfi-27)  ; random-real
(use srfi-13)  ; for string-concatenate

(require "./co")
(require "./util")

(define rand random-real)

;; XY座標から角度を返す
;; ラジアンじゃなくて度
;; 上(Y-)を０として時計回り
(define (get-angle dx dy)
  (rad->deg (atan2 dx (- dy))))

;; 向きと速さから速度を計算
;; 向きは度
;; 上方向が0で時計回り
(define (calc-velocity deg speed)
  (let ((rad (deg->rad deg)))
    (values (* speed    (sin rad))
            (* speed (- (cos rad))))))

;; 角度の正規化 (-180～+180に収める)
(define (degree deg)
  (cond ((> deg 180)
         (- (fmod (+ deg 180) 360) 180))
        ((< deg -180)
         (+ (fmod (- deg 180) 360) 180))
        (else deg)))

;; 文字列として結合
(define (cat . args)
  (string-concatenate (map x->string args)))

;; 結合してシンボルを返す
(define (catsym . args)
  (string->symbol (apply cat args)))

;; ツリーの各ノードを変換する
(define (replace-node s f)
  (cond ((pair? s)
         (cons (replace-node (car s) f)
               (replace-node (cdr s) f)))
        (else (f s))))

;; 変数 $n を (ref $args (- n 1)) に置き換える
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

;; ベクタで保持するメンバへのアクセサを定義
(define-macro (define-accessors class . members)
  (let ((indices (iota (length members))))
    `(begin
       ,@(map (lambda (member idx)
                `(define (,(catsym class '- member) self)
                   (vector-ref self ,idx)))
              members indices)
       ,@(map (lambda (member idx)
                `(define (,(catsym class '- member '-set!) self v)
                   (vector-set! self ,idx v)
                   v))
              members indices))))

;;==========================
;; BulletML タグ処理

(define (action-name sym) (catsym "%action-" sym))
(define (bullet-name sym) (catsym "%bullet-" sym))
(define (fire-name sym)   (catsym "%fire-"   sym))

;; <bulletml> タグ
(define-macro (bulletml . params)
  (define (def-actions actions)
    (map (lambda (action)
           `(,(action-name (get-property (cdr action) :label))
             (lambda (self . $args)
               ,action)))
         actions))
  (define (def-bullets bullets)
    (map (lambda (self)
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
         bullets))
  (define (def-fires fires)
    (map (lambda (fire)
           `(,(fire-name (get-property (cdr fire) :label))
             (lambda (self . $args)
               ,fire)))
         fires))

  (let ((body (get-elem-body params)))
    `(letrec (,@(def-actions (listup-nodes body 'action))
              ,@(def-bullets (listup-nodes body 'bullet))
              ,@(def-fires (listup-nodes body 'fire)))
             ,(action-name 'top))))

;; <action> タグ
(define-macro (action . params)
  (let ((body (get-elem-body params)))
    `(begin ,@body)))

;; <actionRef> タグ
(define-macro (actionRef . params)
  (let ((body (get-elem-body params)))
    (let ((label (action-name (get-property params :label)))
          (args (map (lambda (x)
                       (replace-variable (cadr x)))
                     (listup-nodes body 'param))))
      `(,label self ,@args))))

;; <bullet> タグ
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

;; <repeat> タグ
(define-macro (repeat . params)
  (let ((root `(repeat ,@params)))
    (let ((times (or (aand (find-node params 'times)
                           (replace-variable (cadr it)))
                     1))
          (actions (listup-nodes params 'action))
          (i (gensym)))
      `(dotimes (,i ,times)
         ,@actions))))

;; <fire> タグ
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

;; <fireRef> タグ
(define-macro (fireRef . params)
  (let ((body (get-elem-body params)))
    (let ((label (fire-name (get-property params :label)))
          (args (map (lambda (x)
                       (replace-variable (cadr x)))
                     (listup-nodes body 'param))))
      `(,label self ,@args))))

;; <wait> タグ
(define-macro (wait . params)
  (let ((n (replace-variable (car params))))
    `(yield ,n)))

;; <direction> タグ
(define-macro (direction . params)
  (let ((type (cond ((get-property params :type))
                    (else 'aim)))
        (val (replace-variable (car (get-elem-body params)))))
    (case type
      ((sequence) `(direction-sequence self ,val))
      ((absolute) `(direction-absolute self ,val))
      ((relative) `(direction-relative self ,val))
      (else `(direction-aim self ,val)))))

;; <speed> タグ
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

;; <vanish> タグ
(define-macro (vanish)
  `(vanish-bullet self))

;; <term> タグ
(define-macro (term . params)
  (replace-variable (car params)))

;; <changeDirection> タグ
(define-macro (changeDirection . params)
  (let ((dir (find-node params 'direction))
        (term (find-node params 'term)))
    `(change-direction self
                       ,dir
                       ,term)))

;; <changeSpeed> タグ
(define-macro (changeSpeed . params)
  (let ((speed (find-node params 'speed))
        (term (find-node params 'term)))
    `(change-speed self
                       ,speed
                       ,term)))

;; <accel> タグ
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
;; SML 関連

;; タグを列挙する
(define (listup-nodes nodes tag)
  (if (procedure? tag)
      (filter (lambda (node) (tag (car node)))
              nodes)
    (filter (lambda (node) (eq? (car node) tag))
            nodes)))

;; タグでノードを探す
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

;; プロパティの取得
(define (get-property belem key)
  (let1 r (assoc key (keylist->alist belem))
    (if r
        (cdr r)
      #f)))

;; エレメントのキーワードリストをalistに変換して取得
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

;; エレメントの本体取得
(define (get-elem-body belem)
  (let recur ((ls belem))
    (cond ((null? ls) '())
          ((keyword? (car ls)) (recur (cddr ls)))
          (else ls))))


;;==========================
;; bullet

(define-accessors bullet
                  co
                  x
                  y
                  dir
                  game
                  speed
                  change-dir-term
                  target-dir
                  change-speed-term
                  target-speed
                  vx
                  vy)

(define (make-bullet game x y dir speed proc)
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
                ,game
                ,speed
                ,changeDirTerm
                ,targetDir
                ,changeSpdTerm
                ,targetSpd
                ,vx
                ,vy))))
        (let ((co (make-coroutine proc self)))
          (bullet-co-set! self co))
        self))))

(define (update-bullet self)
  (define (wake-coro) (wake-coroutine (bullet-co self)))
  (define (update-dir)
    (and (> (bullet-change-dir-term self) 0)
         (begin
           (bullet-dir-set! self
                            (+ (bullet-dir self)
                               (/. (degree (- (bullet-target-dir self)
                                              (bullet-dir self)))
                                   (bullet-change-dir-term self))))
           (bullet-change-dir-term-set! self (- (bullet-change-dir-term self) 1))
           #t)))
  (define (update-spd)
    (and (> (bullet-change-speed-term self) 0)
         (begin
           (bullet-speed-set! self
                              (+ (bullet-speed self)
                                 (/ (- (bullet-target-speed self)
                                       (bullet-speed self))
                                    (bullet-change-speed-term self))))
           (bullet-change-speed-term-set! self (- (bullet-change-speed-term self) 1))
           #t)))
  (define (update-velocity)
    (receive (vx vy) (calc-velocity (bullet-dir self) (bullet-speed self))
      (bullet-vx-set! self vx)
      (bullet-vy-set! self vy)))
  (define (move)
    (let ((vx (bullet-vx self))
          (vy (bullet-vy self)))
      (bullet-x-set! self (+ (bullet-x self) vx))
      (bullet-y-set! self (+ (bullet-y self) vy))))
  
  ; 本体
  (wake-coro)
  (when (or (update-dir)
            (update-spd))
    (update-velocity))
  (move))

(define (vanish-bullet self)
  (bullet-x-set! self -100))  ; 画面外に飛ばして消してもらう

(define (change-direction self dir term)
  (bullet-change-dir-term-set! self term)
  (bullet-target-dir-set! self dir))

(define (change-speed self dir term)
  (bullet-change-speed-term-set! self term)
  (bullet-target-speed-set! self dir))

(define (bullet-accel-set! self horz vert term)
  (print "**** bullet-accel-set! not implemented yet"))

;;==========================
;; emitter

(define (make-emitter game x y bml-proc)
  (make-bullet game x y 0 0 bml-proc))

(define (emitter-direction-sequence self ofs)
  (+ (bullet-dir self)
     ofs))

(define (emitter-direction-aim self ofs)
  (let* ((player (game-player (bullet-game self)))
         (ang (get-angle (- (player-x player) (bullet-x self))
                         (- (player-y player) (bullet-y self)))))
    (+ ang ofs)))

(define (emitter-direction-absolute self deg)
  deg)

(define (update-emitter self)
  (wake-coroutine (bullet-co self)))

(define (fire-bullet self bproc dir speed)
  (define (fire-degree-set! self deg)
    (let ((a (degree deg)))
      (bullet-dir-set! self a)
      a))

  ;(print #`"** fire ,bproc ,dir ,speed")
  (let ((game (bullet-game self)))
    (fire-degree-set! self dir)
    (add-bullet game
                (make-bullet game (bullet-x self) (bullet-y self) dir speed bproc))))
