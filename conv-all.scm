(use gauche.process)

(define (conv fn)
  (sys-system #`"gosh bml2sxml.scm ,fn > ,|fn|.scm"))

(define (conv-all fns)
  (dolist (fn fns)
    (print #`"processing ,fn ...")
    (conv fn)))

(conv-all (glob "bullet/*.xml"))
