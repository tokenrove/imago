;;; IMAGO library
;;; Package definition
;;;
;;; Copyright (C) 2020 Vasily Postnicov (shamaz.mazum@gmail.com)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :imago)

(define-condition imago-condition ()
  ()
  (:documentation "Generic imago condition"))

(define-condition imago-error (imago-condition error)
  ()
  (:documentation "Generic imago error"))

(define-condition unknown-format (imago-error)
  ((pathname :initarg :pathname
             :reader  unknown-format-pathname))
  (:documentation "Signaled when trying to write to or read from an image
with unknown format")
  (:report (lambda (c s)
             (format s "Unknown file format: ~a"
                     (unknown-format-pathname c)))))
