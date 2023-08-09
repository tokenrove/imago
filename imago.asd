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

(defsystem :imago/bit-io
  :name :imago/bit-io
  :description "Bit streams for imago library"
  :depends-on (:trivial-gray-streams
               :alexandria
               :serapeum)
  :pathname "bit-io"
  :components ((:file "package")
               (:file "bit-io" :depends-on ("package"))))

(defsystem :imago
  :name "imago"
  :description "Image manipulation library"
  :author "Matthieu Villeneuve <matthieu.villeneuve@free.fr>"
  :license "LLGPL"
  :version "0.10.0"
  :depends-on (:zlib
               :cl-jpeg
               :alexandria
               :serapeum
               :array-operations
               :imago/bit-io
               :flexi-streams)
  :pathname "src"
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
               (:file "contrast" :depends-on ("image" "color"))
               (:file "morphology" :depends-on ("image"))
               (:file "interpolate" :depends-on ("image" "color"))
               (:file "rotate" :depends-on ("image" "color"))
               (:file "operations" :depends-on ("image" "color"))
               (:file "downscale" :depends-on ("image" "color"))
               (:file "file" :depends-on ("conditions"))
               (:file "file-png" :depends-on ("image" "color" "crc32" "file"))
               (:file "file-pnm" :depends-on ("image" "color" "file"))
               (:file "file-tga" :depends-on ("image" "color" "file"))
               #+nil
               (:file "file-pcx" :depends-on ("image" "color" "file"))
               (:file "file-jpg" :depends-on ("image" "color" "file")))
  :in-order-to ((test-op (load-op "imago/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :imago-tests '#:run-tests)))

(defsystem :imago/tests
  :name :imago/tests
  :version "0.10.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "LLGPL"
  :depends-on (:imago :fiveam)
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package"))))

(defsystem :imago/jpeg-turbo
  :name :imago/jpeg-turbo
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "LLGPL"
  :depends-on (:imago :jpeg-turbo)
  :pathname "jpeg-turbo"
  :components ((:file "package")
               (:file "file-jpg" :depends-on ("package"))))

(defsystem :imago/pngio
  :name :imago/pngio
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "LLGPL"
  :depends-on (:imago :pngload :zpng)
  :pathname "pngio"
  :components ((:file "package")
               (:file "file-png" :depends-on ("package"))))

;; An old name of imago/pngio for compatibility
(defsystem :imago/pngload
  :name :imago/pngload
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "LLGPL"
  :depends-on (:imago/pngio))

(defsystem :imago/jupyter
  :name :imago/jupyter
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "LLGPL"
  :depends-on (:imago
               :imago/pngio
               :common-lisp-jupyter
               :cl-base64
               :flexi-streams)
  :pathname "jupyter"
  :components ((:file "package")
               (:file "jupyter" :depends-on ("package"))))
