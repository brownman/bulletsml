(use srfi-13)  ; string-for-each

(define (init-font font-fn fontw fonth)
  (list (load-image font-fn) fontw fonth))

(define (put-char-sub fontsurface x y c fontw fonth)
  (let ((i (- (char->integer c) 32)))
    (receive (q r) (quotient&remainder i 16)
      (let ((u (* r fontw))
            (v (* q fonth)))
        (blit-uv fontsurface x y u v fontw fonth)))))

(define (put-string font x y s)
  (let ((surface (car font))
        (fontw (cadr font))
        (fonth (caddr font)))
    (string-for-each (lambda (c)
                       (put-char-sub surface x y c fontw fonth)
                       (inc! x fontw))
                     s)))
