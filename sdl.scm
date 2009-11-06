;;;
;;; sdl
;;;

(define-module sdl
  (export <SDL_Rect>
          <SDL_Event>
          
          SDL_Init
          SDL_Quit
          SDL_WM_SetCaption
          SDL_GetVideoInfo
          SDL_SetVideoMode
          SDL_LoadBMP
          SDL_SetColorKey
          SDL_DisplayFormat
          SDL_FreeSurface
          SDL_FillRect
          SDL_BlitSurface
          SDL_Flip
          
          SDL_GetTicks
          SDL_Delay
          
          SDL_PollEvent
          
          SDL_INIT_VIDEO
          ; flags for SDL_SetVideoMode
          SDL_HWSURFACE
          SDL_DOUBLEBUF
          SDL_FULLSCREEN
          SDL_RESIZABLE
          SDL_SRCCOLORKEY
          
          ; event
          SDL_KEYDOWN
          SDL_KEYUP
          SDL_QUIT
          SDL_VIDEORESIZE

          ; keysym
          SDLK_ESCAPE
          SDLK_SPACE
          SDLK_UP
          SDLK_DOWN
          SDLK_RIGHT
          SDLK_LEFT
          SDLK_F5
          SDLK_LSHIFT
          SDLK_LCTRL
          SDLK_LAST
          
          
          ; helper functions
          load-image
          blit
          blit-uv
          )
  )
(select-module sdl)

;; Loads extension
(dynamic-load "gauche-sdl")

;;
;; Put your Scheme definitions here
;;

;; 画像読み込み
(define (load-image fn)
  (let1 surface (SDL_LoadBMP fn)
    (if surface
        (begin
          (SDL_SetColorKey surface SDL_SRCCOLORKEY 0)
          (let1 converted (SDL_DisplayFormat surface)
            (if converted
                (begin
                  (SDL_FreeSurface surface)
                  converted)
              surface)))
      #f)))

;; 画像描画
(define blit
  (let ((rc (make <SDL_Rect>)))
    (lambda (dst-surface src-surface x y)
      (slot-set! rc 'x x)
      (slot-set! rc 'y y)
      (SDL_BlitSurface src-surface '() dst-surface rc))))

;; UV指定つき画像描画
(define blit-uv
  (let ((srcrc (make <SDL_Rect>))
        (dstrc (make <SDL_Rect>)))
    (lambda (dst-surface src-surface x y u v w h)
      (slot-set! dstrc 'x x)
      (slot-set! dstrc 'y y)
      (slot-set! srcrc 'x u)
      (slot-set! srcrc 'y v)
      (slot-set! srcrc 'w w)
      (slot-set! srcrc 'h h)
      (SDL_BlitSurface src-surface srcrc dst-surface dstrc))))

;; Epilogue
(provide "sdl")
