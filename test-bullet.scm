;; BulletSML を使ったテスト

(use sdl)
(load "./bulletsml")

(define-constant ScreenWidth  300)
(define-constant ScreenHeight 400)
(define-once *rank* 100)

;; 画面外？
(define (out-of-screen? x y size)
  (or (< x (- size))
      (> x (+ ScreenWidth size))
      (< y (- size))
      (> y (+ ScreenHeight size))))

;;==========================
;; bullet

(define (render-bullet bullet dst-surface)
  (blit dst-surface (get-image 'bullet)
        (to-int (window-scale (- (bullet-x bullet) 4)))
        (to-int (window-scale (- (bullet-y bullet) 4)))))

;;==========================
;; player

(define (make-player x y)
  (let ((damaged #f))
    (list->vector
     `(,x
       ,y
       ,damaged))))

(define (player-x player)
  (vector-ref player 0))

(define (player-y player)
  (vector-ref player 1))

(define (player-damaged? player)
  (vector-ref player 2))

(define (player-set-damage player)
  (vector-set! player 2 (* 2 60)))

(define MoveTbl
  (let* ((D 2)
         (-D (- D))
         (R (* D 1.41421356 0.5))
         (-R (- R)))
    (vector
     (vector  0  0)
     (vector -D  0)
     (vector  D  0)
     (vector  0  0)
     (vector  0 -D)
     (vector -R -R)
     (vector  R -R)
     (vector  0 -D)
     (vector  0  D)
     (vector -R  R)
     (vector  R  R)
     (vector  0  D)
     (vector  0  0)
     (vector -D  0)
     (vector  D  0)
     (vector  0  0))))

(define (update-player self)
  (define (move)
    (let* ((size/2 8)
           (xmin size/2)
           (ymin size/2)
           (xmax (- ScreenWidth size/2))
           (ymax (- ScreenHeight size/2)))
      (let* ((idx (+ (if (key-pressed? SDLK_LEFT)  1 0)
                     (if (key-pressed? SDLK_RIGHT) 2 0)
                     (if (key-pressed? SDLK_UP)    4 0)
                     (if (key-pressed? SDLK_DOWN)  8 0)))
             (tbl (vector-ref MoveTbl idx)))
        (let ((x (clamp (+ (vector-ref self 0) (vector-ref tbl 0))
                        xmin xmax))
              (y (clamp (+ (vector-ref self 1) (vector-ref tbl 1))
                        ymin ymax)))
          (vector-set! self 0 x)
          (vector-set! self 1 y)))))
  (define (decrement-damage-count)
    (when (vector-ref self 2)
      (dec! (vector-ref self 2))
      (when (zero? (vector-ref self 2))
        (vector-set! self 2 #f))))
  
  (decrement-damage-count)
  (move))

(define (render-player self dst-surface)
  (let ((damaged (vector-ref self 2)))
    (when (or (not damaged)
              (< (modulo damaged 8) 4))
      (blit dst-surface (get-image 'player)
            (to-int (window-scale (- (player-x self) 8)))
            (to-int (window-scale (- (player-y self) 8)))))))

;;==========================
;; game

(define (make-game)
  (let ((bullets '())
        (player (make-player (/ ScreenWidth 2)
                             (* ScreenHeight 7/8))))
    (list->vector
     `(,player
       ,bullets
       0))))

(define (game-player game)
  (vector-ref game 0))

(define (game-bullet-count game)
  (vector-ref game 2))

(define (add-bullet game bullet)
  (vector-set! game 1
               (cons bullet
                     (vector-ref game 1)))
  (inc! (vector-ref game 2)))

(define (remove-bullet game bullet)
  (vector-set! game 1
               (remove-from-list! bullet (vector-ref game 1)))
  (dec! (vector-ref game 2))
  (delete-bullet bullet))

(define (update-rank)
  (let ((rank (cond ((key-pressed? #\q) (- *rank* 1))
                    ((key-pressed? #\w) (+ *rank* 1))
                    (else *rank*))))
    (set! *rank* (clamp rank 0 100))))

(define (rect-overlap? x0 y0 w0 h0 x1 y1 w1 h1)
  (and (< x0 (+ x1 w1))
       (< y0 (+ y1 h1))
       (< x1 (+ x0 w0))
       (< y1 (+ y0 h0))))

(define (check-player-bullets-collision player bullets)
  (when (not (player-damaged? player))
    (dolist (bullet bullets)
      (when (rect-overlap? (player-x player) (player-y player) 1 1
                           (- (bullet-x bullet) 3) (- (bullet-y bullet) 3) 6 6)
        (player-set-damage player)))))

(define (update-game game)
  (define (update-bullets)
    (let recur ((ls  (vector-ref game 1)))
      (when (not (null? ls))
        (let ((bullet (car ls))
              (next (cdr ls)))
          (update-bullet bullet)
          (when (out-of-screen? (bullet-x bullet)
                                (bullet-y bullet)
                                16)
            (remove-bullet game bullet))
          (recur next)))))

  (update-rank)
  
  (update-player (game-player game))
  (update-bullets)
  (check-player-bullets-collision (game-player game)
                                  (vector-ref game 1))
  )

(define (render-game game dst-surface)
  (render-player (game-player game) dst-surface)
  (dolist (bullet (vector-ref game 1))
    (render-bullet bullet dst-surface)))

(define (rank)
  (* *rank* 0.01))
