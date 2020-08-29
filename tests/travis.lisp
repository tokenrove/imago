(defun do-all()
  (if (string= "YES" (uiop:getenv "WITH_JPEG_TURBO"))
      (ql:quickload :imago/jpeg-turbo))
  (ql:quickload :imago/tests)
  (uiop:quit
   (if (uiop:call-function "imago-tests:run-tests")
        0 1)))

(do-all)
