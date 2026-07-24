(defun do-all()
  (handler-case
      (progn
        (asdf:load-system :imago/jpeg-turbo)
        (asdf:load-system :imago/libheif)
        (asdf:load-system :imago/libtiff)
        (asdf:load-system :imago/tests))
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "imago/tests:run-tests")
       0 1)))

(do-all)
