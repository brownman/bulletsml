;; BulletSML を使ったテスト

(use sdl)
(load "./bulletsml")

(define-constant ScreenWidth  300)
(define-constant ScreenHeight 400)

;; 画面外？
(define (out-of-screen? x y size)
  (or (< x (- size))
      (> x (+ ScreenWidth size))
      (< y (- size))
      (> y (+ ScreenHeight size))))

;;==========================
;; player

(define (make-player)
  (let ((x (/ ScreenWidth 2))
        (y (- ScreenHeight (/ ScreenHeight 8))))
    (list->vector
     `(,x
       ,y))))

(define (player-x player)
  (vector-ref player 0))

(define (player-y player)
  (vector-ref player 1))

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
  (let ((size/2 8))
    (let* ((idx (+ (if (key-pressed? SDLK_LEFT)  1 0)
                   (if (key-pressed? SDLK_RIGHT) 2 0)
                   (if (key-pressed? SDLK_UP)    4 0)
                   (if (key-pressed? SDLK_DOWN)  8 0)))
           (tbl (vector-ref MoveTbl idx)))
      (inc! (vector-ref self 0) (vector-ref tbl 0))
      (inc! (vector-ref self 1) (vector-ref tbl 1)))
    
    (when (< (player-x self) size/2)
      (vector-set! self 0 size/2))
    (when (> (player-x self) (- ScreenWidth size/2))
      (vector-set! self 0 (- ScreenWidth size/2)))
    (when (< (player-y self) size/2)
      (vector-set! self 1 size/2))
    (when (> (player-y self) (- ScreenHeight size/2))
      (vector-set! self 1 (- ScreenHeight size/2)))))

(define (render-player player dst-surface sprite)
  (blit dst-surface sprite
        (to-int (- (player-x player) 8))
        (to-int (- (player-y player) 8))))

(define (render-bullet bullet dst-surface sprite)
  (blit dst-surface sprite
        (to-int (- (bullet-x bullet) 4))
        (to-int (- (bullet-y bullet) 4))))

;;==========================
;; game

(define (make-game)
  (let ((emitters '())
        (bullets '())
        (player (make-player)))
    (list->vector
     `(,player
       ,emitters
       ,bullets))))

(define (game-player game)
  (vector-ref game 0))

(define (add-emitter game emitter)
  (vector-set! game 1
               (cons emitter
                     (vector-ref game 1))))

(define (add-bullet game bullet)
  (vector-set! game 2
               (cons bullet
                     (vector-ref game 2))))

(define (remove-bullet game bullet)
  (vector-set! game 2
               (remove-from-list! bullet (vector-ref game 2))))

(define (update-game game)
  (define (update-emitters)
    (dolist (emitter (vector-ref game 1))
      (update-emitter emitter)))
  (define (update-bullets)
    (let recur ((ls  (vector-ref game 2)))
      (when (not (null? ls))
        (let ((bullet (car ls))
              (next (cdr ls)))
          (update-bullet bullet)
          (when (out-of-screen? (bullet-x bullet)
                                (bullet-y bullet)
                                16)
            (remove-bullet game bullet))
          (recur next)))))

  (update-player (game-player game))
  (update-emitters)
  (update-bullets))

(define (render-game game dst-surface shot-sprite player-sprite)
  (render-player (game-player game) dst-surface player-sprite)
  (dolist (bullet (vector-ref game 2))
    (render-bullet bullet dst-surface shot-sprite)))

(define (rank)
  0.5)


;;==========================

(define bullet2
  (bulletml
   (action :label top
           (repeat
            (times 3)
            (action
             (fire
              (direction :type sequence
                         23)
              (bullet))
             (wait 1))))))

(define bullet3
  (bulletml
   (action :label top
           (repeat
            (times 10)
            (action
             (fire
              (direction :type sequence
                         23)
              (bulletRef :label straight))
             (wait 1))))
   (bullet :label straight
           (action
            (wait (+ 20 (* (rand) 50)))
            (changeDirection
             (direction :type absolute
                        180)
             (term 10))))))

(define hibachi_1
  (bulletml
   (action :label top
           (repeat
            (times (+ 10 (* (rank) 70)))
            (action
             (fire
              (direction :type aim (+ (* (rand) 30) -74 (* (rank) 2)))
              (speed (+ 0.5 (* (rank) 2)))
              (bullet))
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (fireRef :label n)
             (wait (- 14 (* (rank) 10))))))
   (fire :label n
         (direction :type sequence
                    (+ (* (rand) 2) 7 (- (* (rank) 2))))
         (speed (+ 0.5 (* (rank) 2)))
         (bullet))))

(define bulletsmorph2
  (bulletml :type vertical
            (action :label top
                    (repeat
                     (times 8)
                     (action
                      (actionRef :label center
                                 (param (* 90 (rand)))
                                 (param 1))
                      (wait 12)
                      (actionRef :label center
                                 (param (* 90 (rand)))
                                 (param -1))
                      (wait 12)
                      (actionRef :label center
                                 (param (* 30 (rand)))
                                 (param 1))
                      (wait 12)
                      (actionRef :label center
                                 (param (* 30 (rand)))
                                 (param -1))
                      (wait 12)))
                    (wait 150))
            (action :label center
                    (fire
                     (direction :type absolute
                                (* 360 (rand)))
                     (bulletRef :label circle
                                (param $1)
                                (param $2)))
                    (repeat
                     (times (- (+ 4 (* 8 (rank))) 1))
                     (action
                      (fire
                       (direction :type sequence
                                  (/ 360 (+ 4 (* 8 (rank)))))
                       (bulletRef :label circle
                                  (param $1)
                                  (param $2))))))
            (bullet :label circle
                    (speed 1.3)
                    (action
                     (wait 20)
                     (changeDirection
                      (direction :type absolute
                                 (+ 180 (* $1 $2)))
                      (term 1))
                     (wait (- 125 $1))
                     (fire
                      (direction :type aim 0)
                      (bulletRef :label red))
                     (vanish)))
            (bullet :label red
                    (speed 0.1)
                    (action
                     (changeSpeed
                      (speed 4.0)
                      (term 300))))))

(define ketui_lt_1boss_bit
(bulletml :type vertical
 (bullet :label Dummy
  (action
   (vanish)))
 (action :label XWay
  (actionRef :label XWayFan
   (param $1)
   (param $2)
   (param 0)))
 (action :label XWayFan
  (repeat
   (times (- $1 1))
   (action
    (fire
     (direction :type sequence $2)
     (speed :type sequence $3)
     (bullet)))))
 (action :label |3way|
  (repeat
   (times 2)
   (action
    (wait 30)
    (fire
     (direction :type aim -3)
     (speed 1.4)
     (bullet))
    (actionRef :label XWay
     (param 3)
     (param 2)))))
 (bullet :label bit
  (action
   (repeat
    (times 3)
    (action
     (accel
      (horizontal :type absolute 0)
      (vertical :type absolute 1)
      (term 60))
     (actionRef :label |3way|)
     (accel
      (horizontal :type absolute -2)
      (vertical :type absolute 0)
      (term 60))
     (actionRef :label |3way|)
     (accel
      (horizontal :type absolute 0)
      (vertical :type absolute -1)
      (term 60))
     (actionRef :label |3way|)
     (accel
      (horizontal :type absolute 2)
      (vertical :type absolute 0)
      (term 60))
     (actionRef :label |3way|)))))
 (action :label top
  (repeat
   (times (+ 4 (* (rank) 6)))
   (action
    (fire
     (direction :type absolute 90)
     (speed 2)
     (bulletRef :label bit))
    (wait (/ 245 (+ 4 (* (rank) 6))))))
  (wait 550))))

(define (test)
  (let ((game (make-game)))
    (add-emitter game (make-emitter game 150 50
                                    ;bullet2))
                                    ;bullet3))
                                    ;hibachi_1))
                                    bulletsmorph2))
                                    ;ketui_lt_1boss_bit))
    (dotimes (i 100)
      (print `(===== ,i =====))
      (update-game game))
    ;(write/ss game) (newline)
    ))

;(define (main args) (test))
