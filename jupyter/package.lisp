(defpackage imago-jupyter
  (:use #:cl
        #:imago
        #:jupyter
        #:base64)
  (:local-nicknames (:imago-jt :imago-jpeg-turbo))
  (:export #:show-image))
