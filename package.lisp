;;; IMAGO library
;;; Package definition
;;;
;;; Copyright (C) 2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package #:cl-user)

(defpackage :imago
  (:use :common-lisp)
  (:export #:image
           #:image-width #:image-height
           #:image-pixels #:image-pixel
           #:rgb-image #:indexed-image #:grayscale-image
           #:rgb-pixel #:indexed-pixel #:grayscale-pixel

           #:with-image-definition
           #:do-image-pixels #:do-region-pixels #:do-line-pixels
           #:set-alpha

           #:make-color
           #:color-red #:color-green #:color-blue #:color-alpha
           #:color-rgb #:color-argb
           #:color-intensity
           #:invert-color
           #:make-gray
           #:gray-intensity #:gray-alpha
           #:invert-gray
           #:+white+ #:+black+
           #:+red+ #:+green+ #:+blue+
           #:+cyan+ #:+magenta+ #:+yellow+

           #:convert-to-rgb #:convert-to-indexed #:convert-to-grayscale

           #:copy
           #:flip #:scale #:resize

           #:draw-pixel #:draw-line #:draw-rectangle #:draw-circle

           #:convolve
           #:blur #:sharpen #:edge-detect #:emboss

           #:compose

           #:read-png #:write-png
           #:read-pnm #:write-pnm
           #:read-tga #:write-tga))


