(defpackage imago-jpeg-turbo
  (:use #:cl #:jpeg-turbo)
  (:export #:read-jpg
           #:write-jpg

           #:read-jpg-from-octets
           #:write-jpg-to-octets))
