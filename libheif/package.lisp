(defpackage imago/libheif
  (:use #:cl #:cl-libheif)
  (:local-nicknames (#:ff #:float-features))
  (:export #:*heic-threads* #:read-heic #:write-heic))
