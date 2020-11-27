(defun do-all()
  (when (string= "YES" (uiop:getenv "WITH_ALTIO"))
    (ql:quickload :imago/jpeg-turbo)
    (ql:quickload :imago/pngload))
  (ql:quickload :imago/tests)
  (uiop:quit
   (if (uiop:call-function "imago-tests:run-tests")
        0 1)))

(do-all)
