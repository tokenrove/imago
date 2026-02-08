(defun do-all()
  (handler-case
      (progn
        (when (string= "YES" (uiop:getenv "WITH_ALTIO"))
          (asdf:load-system :imago/jpeg-turbo)
          (asdf:load-system :imago/libheif)
          (asdf:load-system :imago/pngload))
        (asdf:load-system :imago/tests))
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "imago-tests:run-tests")
       0 1)))

(do-all)
