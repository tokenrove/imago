;;; IMAGO library
;;; ASDF system definition
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;; Copyright (C) 2020 Vasily Postnicov (shamaz.mazum@gmail.com)
;;;
;;; The author grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :asdf-user)

(defsystem imago
  :name "imago"
  :description "Image manipulation library"
  :author "Matthieu Villeneuve <matthieu.villeneuve@free.fr>"
  :license "LLGPL"
  :version "0.9.0"
  :depends-on (:zlib :cl-jpeg)
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "utilities" :depends-on ("package"))
               (:file "color" :depends-on ("package"))
               (:file "image" :depends-on ("package"))
               (:file "image-utilities" :depends-on ("image" "color" "utilities"))
               (:file "crc32" :depends-on ("package"))
               (:file "drawing" :depends-on ("image-utilities" "color"))
               (:file "convert" :depends-on ("image" "color"))
               (:file "convolve" :depends-on ("image" "color"))
               (:file "compose" :depends-on ("image" "color"))
               (:file "operations" :depends-on ("image" "color"))
               (:file "file" :depends-on ("conditions"))
               (:file "file-png" :depends-on ("image" "color" "crc32" "file"))
               (:file "file-pnm" :depends-on ("image" "color" "file"))
               (:file "file-tga" :depends-on ("image" "color" "file"))
               (:file "file-pcx" :depends-on ("image" "color" "file"))
               (:file "file-jpg" :depends-on ("image" "color" "file")))
  :in-order-to ((test-op (load-op "imago/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :imago-tests '#:run-tests)))

(defsystem :imago/tests
  :name :imago/tests
  :version "0.9.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "LLGPL"
  :depends-on (:imago :fiveam)
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests" :depends-on ("package"))))
