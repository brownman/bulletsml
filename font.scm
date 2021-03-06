;; フォント描画

(use sdl)
(use srfi-13)  ; string-for-each

(define (init-font font-surface fontw fonth)
  (list font-surface fontw fonth))

(define (put-char-sub dst-surface fontsurface x y c fontw fonth)
  (let ((i (- (char->integer c) 32)))
    (receive (q r) (quotient&remainder i 16)
      (let ((u (* r fontw))
            (v (* q fonth)))
        (blit-uv dst-surface fontsurface x y u v fontw fonth)))))

(define (put-string font dst-surface x y s)
  (let ((surface (car font))
        (fontw (cadr font))
        (fonth (caddr font)))
    (string-for-each (lambda (c)
                       (put-char-sub dst-surface surface x y c fontw fonth)
                       (inc! x fontw))
                     s)))
