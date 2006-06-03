;;; IMAGO library
;;; Package definition
;;;
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
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
           #:image-plane-count #:image-colormap
           #:image-pixels #:image-pixel
           #:rgb-image #:indexed-image #:grayscale-image #:planar-image
           #:rgb-pixel #:indexed-pixel #:grayscale-pixel #:planar-pixel

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
           #:convert-to-planar

           #:copy
           #:flip #:scale #:resize

           #:draw-pixel #:draw-line
           #:draw-rectangle #:draw-polygon
           #:draw-circle
           #:draw-bezier-curve

           #:convolve
           #:blur #:sharpen #:edge-detect #:emboss

           #:compose

           #:read-image
           #:register-image-reader

           #:read-png #:write-png
           #:read-pnm #:write-pnm
           #:read-tga #:write-tga
           #:read-pcx #:write-pcx))


