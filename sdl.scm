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
          )
  )
(select-module sdl)

;; Loads extension
(dynamic-load "gauche-sdl")

;;
;; Put your Scheme definitions here
;;

;; Epilogue
(provide "sdl")
