(use gauche.process)

(define (conv fn)
  (sys-system #`"gosh bml2sxml.scm ,fn > ,|fn|.scm"))

(define (conv-all fns)
  (dolist (fn fns)
    (print #`"processing ,fn ...")
    (conv fn)))

;(print
; (let1 proc (run-process '("gosh" "bml2sxml.scm" "[Bulletsmorph]_aba_1.xml") :output :pipe :wait #t)
;   (read (process-output proc))))

;(sys-system "gosh bml2sxml.scm [Bulletsmorph]_aba_1.xml > err")

(conv-all (glob "bullet/*.xml"))
