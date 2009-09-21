(define (/= a b) (not (= a b)))

(define ^ lambda)

(define-macro (define-once name . body)
  `(unless (global-variable-bound? (current-module) ',name)
     (define ,name ,@body)))

(define-macro (w/uniq names . body)
  (if (pair? names)
      `(let (map (^(n) (list n '(gensym)))
                 names)
         ,@body)
    `(let1 ,names (gensym)
       ,@body)))

(define-macro (iflet var val then . rest)
  (w/uniq g
    `(let1 ,g ,val
       (if ,g
           (let1 ,var ,g
             ,then)
         ,@rest))))

(define-macro (vcase val . rest)
  (w/uniq g
    `(let1 ,g ,val
       (cond ,@(map (^(exp)
                      (if (eq? (car exp) 'else)
                          exp
                        `((= ,(car exp) ,g) ,@(cdr exp))))
                    rest)))))

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

(load "./font")

(define-constant FRAME-TICKS (quotient 1000 60))
(define-constant ScreenWidth 300)
(define-constant ScreenHeight 400)

(define-once *screen* '())
(define-once videoflags '())
(define-once video_bpp '())

(define-once *font* '())

(define (terminate)
  (SDL_Quit))

(define (init caption w h)
  (if (< (SDL_Init SDL_INIT_VIDEO) 0)
      #f
    (begin
      (SDL_WM_SetCaption caption '())
      
      (let* ((info (SDL_GetVideoInfo))
             (bpp (slot-ref info 'vfmt->BitsPerPixel)))
        (set! video_bpp (if (> bpp 8)
                            bpp
                          16)))
      
      (set! videoflags (logior SDL_HWSURFACE SDL_DOUBLEBUF))
      ;(set! videoflags (logior videoflags SDL_FULLSCREEN))
      (set! *screen* (SDL_SetVideoMode w h video_bpp videoflags))
      *screen*)))

(define frame-wait
  (let ((lastticks #f))
    (^(frame-ticks)
      (let1 ticks (SDL_GetTicks)
        (if lastticks
            (let1 d (- frame-ticks (- ticks lastticks))
              (cond ((>= d 0)
                     (SDL_Delay d)
                     (inc! lastticks frame-ticks))
                    (else
                     (set! lastticks ticks))))
          (set! lastticks ticks))))))

(define proc-events
  (let ((event (make <SDL_Event>)))
    (^()
      (let1 cont #t
        (while (SDL_PollEvent event)
          (vcase (slot-ref event 'type)
                 (SDL_QUIT         (set! cont #f))
                 (SDL_VIDEORESIZE  (SDL_SetVideoMode (slot-ref event 'resize.w) (slot-ref event 'resize.h) video_bpp videoflags))
                 (SDL_KEYDOWN      (keydown (slot-ref event 'key.keysym.sym)))
                 (SDL_KEYUP        (keyup   (slot-ref event 'key.keysym.sym)))
                 ))
        cont))))

(define (load-image fn)
  (iflet surface (SDL_LoadBMP fn)
      (begin
#|
        (when (slot-ref surface 'pixels)
          (SDL_SetColorKey surface
                           SDL_SRCCOLORKEY
                           (SDL_Surface-pixels-ref surface 0)))
|#
        (iflet converted (SDL_DisplayFormat surface)
            (begin
              (SDL_FreeSurface surface)
              converted)
          surface))
    #f))

(define blit
  (let ((rc (make <SDL_Rect>)))
    (^(src-surface x y)
      (slot-set! rc 'x x)
      (slot-set! rc 'y y)
      (SDL_BlitSurface src-surface '() *screen* rc))))

(define blit-uv
  (let ((srcrc (make <SDL_Rect>))
        (dstrc (make <SDL_Rect>)))
    (^(src-surface x y u v w h)
      (slot-set! dstrc 'x x)
      (slot-set! dstrc 'y y)
      (slot-set! srcrc 'x u)
      (slot-set! srcrc 'y v)
      (slot-set! srcrc 'w w)
      (slot-set! srcrc 'h h)
      (SDL_BlitSurface src-surface srcrc *screen* dstrc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./test-game")

(define (loop shot-sprite player-sprite)
  (define (main-loop)
    (let ((game (make-game))
          (cnt 0))
      (add-emitter game (make-emitter game
                                      hibachi_1))
      (call/cc
       (^(break)
         (while (proc-events)
           (when (key-pressed? SDLK_ESCAPE)
             (break))
           (when (key-pressed? SDLK_F5)
             (break (lambda ()
                      (reload)
                      (again))))
           (SDL_FillRect *screen* '() 0)
           
           (update-game game)
           (render-game game shot-sprite player-sprite)
           
           (disp-time cnt)
           (inc! cnt)
           
           (frame-wait FRAME-TICKS)
           (SDL_Flip *screen*))))))

  (define (reload)
    (print "==== reload ====")
    (load "./entry"))
  
  (define (again)
    (loop shot-sprite player-sprite))
  
  (define (disp-time t)
    (let ((sec (modulo (quotient t 60) 60))
          (min (quotient t 3600))
          (c (if (< (modulo t 60) 30) ":" " ")))
      (put-string *font* 0 0 (format #f "TIME ~2,'0d~a~2,'0d" min c sec))))
  
  (let ((res (main-loop)))
    (when (procedure? res)
      (res))))

#|
  18x30ƒhƒbƒg
|#

(define (main args)
  (if (init "BulletSML" ScreenWidth ScreenHeight)
      (let ((shot-sprite (load-image "data/bullet.bmp"))
            (player-sprite (load-image "data/player.bmp")))
        (set! *font* (init-font "data/font.bmp" 8 8))
        (loop shot-sprite player-sprite)
        (SDL_FreeSurface shot-sprite)
        (SDL_FreeSurface player-sprite)
        (terminate))
    (print "failed")))
