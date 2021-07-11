(defpackage imago-jupyter
  (:use #:cl
        #:imago
        #:jupyter
        #:flexi-streams
        #:base64)
  (:export #:show-image))
