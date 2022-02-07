;;; IMAGO library
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;; Copyright (C) 2022  Vasily Postnicov (shamaz.mazum@gmail.com)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(defpackage imago-bit-io
  (:use #:cl)
  (:local-nicknames (:gray :trivial-gray-streams)
                    (:alex :alexandria)
                    (:sera :serapeum))
  (:export #:bit-stream
           #:bit-input-stream
           #:bit-output-stream

           #:read-bit
           #:skip-to-byte-alignment))
