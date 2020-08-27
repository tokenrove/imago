(defun do-all()
  (ql:quickload :imago/tests)
  (uiop:quit
   (if (uiop:call-function "imago-tests:run-tests")
        0 1)))

(do-all)
