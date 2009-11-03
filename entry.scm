
(load "./util")
(load "./font")
(load "./test-bullet")

(define-macro (define-once name . body)
  `(unless (global-variable-bound? (current-module) ',name)
     (define ,name ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use sdl)

(use gauche.uvector)
(define *key-state* (make-u8vector SDLK_LAST 0))

(define (keydown keysym)
  (u8vector-set! *key-state* keysym 1))

(define (keyup keysym)
  (u8vector-set! *key-state* keysym 0))

(define (key-pressed? keysym)
  (/= (u8vector-ref *key-state* keysym) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant FRAME-TICKS (quotient 1000 60))
(define-constant ScreenWidth 300)
(define-constant ScreenHeight 400)

(define-once *screen* '())
(define-once *videoflags* '())
(define-once *video_bpp* '())

(define-once *font* '())

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
  (let ((lastticks #f))
    (lambda (frame-ticks)
      (let1 ticks (SDL_GetTicks)
        (if lastticks
            (let1 d (- frame-ticks (- ticks lastticks))
              (cond ((>= d 0)
                     (SDL_Delay d)
                     (inc! lastticks frame-ticks))
                    (else
                     (set! lastticks ticks))))
          (set! lastticks ticks))))))

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
(define (disp-time t)
  (let ((sec (modulo (quotient t 60) 60))
        (min (quotient t 3600))
        (c (if (< (modulo t 60) 30) ":" " ")))
    (put-string *font* *screen* 0 0 (format #f "TIME ~2,'0d~a~2,'0d" min c sec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ゲーム開始
(define (start shot-sprite player-sprite)
  ; メインループ
  (define (main-loop break)
    (let ((game (make-game))
          (cnt 0))
      (add-emitter game (make-emitter game
                                      150 50
                                      ;hibachi_1))
                                      bulletsmorph2))
      (while (proc-events)
        (when (key-pressed? SDLK_ESCAPE)
          (break))
        (when (key-pressed? SDLK_F5)
          (break (lambda ()
                   (reload)
                   (again))))
        (SDL_FillRect *screen* '() 0)
        
        (update-game game)
        (render-game game *screen* shot-sprite player-sprite)
        
        (disp-time cnt)
        (inc! cnt)
        
        (frame-wait FRAME-TICKS)
        (SDL_Flip *screen*))))
  
  (define (reload)
    (print "==== reload ====")
    (load "./entry"))
  
  (define (again)
    (start shot-sprite player-sprite))
  
  (let ((res (call/cc
              (lambda (break)
                (main-loop break)))))
    (when (procedure? res)
      (res))))


;; エントリ
(define (main args)
  (if (init "BulletSML Test" ScreenWidth ScreenHeight)
      (let ((shot-sprite (load-image "data/bullet.bmp"))
            (player-sprite (load-image "data/player.bmp")))
        (set! *font* (init-font "data/font.bmp" 8 8))
        (start shot-sprite player-sprite)
        (SDL_FreeSurface shot-sprite)
        (SDL_FreeSurface player-sprite)
        (terminate))
    (print "failed")))
