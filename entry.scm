
(load "./util")
(load "./font")
(load "./test-bullet")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use sdl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーの状態

(use gauche.uvector)
(define *key-state* (make-u8vector SDLK_LAST 0))
  
(define (to-keysym keysym)
  (if (char? keysym)
      (char->integer keysym)
    keysym))

(define (keydown keysym)
  (u8vector-set! *key-state* keysym 1))

(define (keyup keysym)
  (u8vector-set! *key-state* keysym 0))

(define (key-pressed? keysym)
  (/= (u8vector-ref *key-state* (to-keysym keysym)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; リソース管理

(define-once *images* (make-hash-table))

(define (load-images path fns)
  (dolist (fn fns)
    (let ((surface (load-image (cat path fn ".bmp"))))
      (hash-table-put! *images* fn surface))))

(define (free-images)
  (hash-table-for-each *images*
                       (lambda (_ surface)
                         (SDL_FreeSurface surface))))

(define (get-image key)
  (hash-table-get *images* key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (window-scale x) (* x 2))

(define-constant FRAME-TICKS (/ 1000.0 60))
(define-constant ScreenWidth 300)
(define-constant ScreenHeight 400)

(define-once *screen* '())
(define-once *videoflags* '())
(define-once *video_bpp* '())

(define-once *font* '())
(define *current-bullet* "*none*")

;; 後始末
(define (terminate)
  (SDL_Quit))

;; 初期化
(define (init caption w h)
  (if (< (SDL_Init SDL_INIT_VIDEO) 0)
      #f
    (begin
      (SDL_WM_SetCaption caption '())
      
      (let* ((info (SDL_GetVideoInfo))
             (bpp (slot-ref info 'vfmt->BitsPerPixel)))
        (set! *video_bpp* (if (> bpp 8)
                            bpp
                          16)))
      
      (set! *videoflags* (logior SDL_HWSURFACE SDL_DOUBLEBUF))
      ;(set! *videoflags* (logior *videoflags* SDL_FULLSCREEN))
      (set! *screen* (SDL_SetVideoMode w h *video_bpp* *videoflags*))
      *screen*)))

;; 時間待ち
(define frame-wait
  (let ((lastticks (SDL_GetTicks)))
    (lambda (wait-ms)
      (let1 ticks (SDL_GetTicks)
        (let1 d (- wait-ms (- ticks lastticks))
          (cond ((>= d 0)
                 (SDL_Delay (floor->exact d))
                 (inc! lastticks wait-ms))
                (else
                 (set! lastticks ticks))))))))

;; FPS計測
(define measure-fps
  (let ((lastticks (SDL_GetTicks))
        (ndraw 0)
        (fps 0))
    (lambda ()
      (inc! ndraw)
      (let1 ticks (SDL_GetTicks)
        (let1 d (- ticks lastticks)
          (when (>= d 1000)
            (set! fps (floor (/ (* ndraw 1000) d)))
            (set! ndraw 0)
            (set! lastticks ticks))))
      fps)))

;; SDLイベントを処理する
(define proc-events
  (let ((event (make <SDL_Event>)))
    (lambda ()
      (let1 cont #t
        (while (SDL_PollEvent event)
          (vcase (slot-ref event 'type)
                 (SDL_QUIT         (set! cont #f))
                 (SDL_VIDEORESIZE  (SDL_SetVideoMode (slot-ref event 'resize.w) (slot-ref event 'resize.h) *video_bpp* *videoflags*))
                 (SDL_KEYDOWN      (keydown (slot-ref event 'key.keysym.sym)))
                 (SDL_KEYUP        (keyup   (slot-ref event 'key.keysym.sym)))
                 ))
        cont))))

;; 時間の表示
(define (disp-fps fps)
  (put-string *font* *screen* 0 0 (format #f "FPS ~a" fps)))

;; 弾の数表示
(define (disp-bullet-count game)
  (let ((num (game-bullet-count game)))
    (put-string *font* *screen* 100 0 (format #f "#BULLET:~a" num))))

;; ランクの表示
(define (disp-rank)
  (put-string *font* *screen* 200 0 (format #f "RANK:~a" *rank*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-select ls)
  (let1 idx (random-integer (vector-length ls))
    (ref ls idx)))

(define (make-emitter bullets)
  (lambda (self)
    (while #t
      (let ((pat (random-select bullets)))
        (print pat)
        (set! *current-bullet* (car pat))
        ((cdr pat) self)
        (wait (* 3 60))))))

;; ゲーム開始
(define (start bullets)
  (let again ()
    ; メインループ
    (define (main-loop break)
      (let ((game (make-game)))
        (add-bullet game (make-bullet game
                                      150 50 0 0
                                      ;hibachi_1))
                                      (make-emitter bullets)))
        (while (proc-events)
          (when (key-pressed? SDLK_ESCAPE)
            (break))
          (when (key-pressed? SDLK_F5)
            (break (lambda ()
                     (reload)
                     (again))))
          (SDL_FillRect *screen* '() 0)
          
          (update-game game)
          (render-game game *screen*)
          
          (disp-bullet-count game)
          (disp-rank)
          (put-string *font* *screen* 0 10 *current-bullet*)
          
          (frame-wait FRAME-TICKS)
          (disp-fps (measure-fps))
          (SDL_Flip *screen*))))
    
    (define (reload)
      (print "==== reload ====")
      (load "./entry"))
    
    (let ((res (call/cc
                (lambda (break)
                  (main-loop break)))))
      (when (procedure? res)
        (res)))))

;; 弾幕ファイル読み込み
(define (load-bullets)
  (list->vector
   (filter (lambda (x) x)
           (map (lambda (fn)
                  (print #`"loading ,fn ...")
                  (let1 text (call-with-input-file fn read)
                    (if (eof-object? text)
                        #f
                      (cons fn
                            (eval text interaction-environment)))))
                (glob "bullet/*.scm")))))

;; エントリ
(define (main args)
  (if (init "BulletSML Test" (window-scale ScreenWidth) (window-scale ScreenHeight))
      (begin
        (load-images "data/"
                     '(font
                       player
                       bullet))
        (set! *font* (init-font (get-image 'font) 8 8))
        (start (load-bullets))
        (free-images)
        (terminate))
    (print "failed")))
