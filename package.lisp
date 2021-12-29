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
  (:use #:common-lisp #:polymorphic-functions)
  (:local-nicknames (:alex :alexandria)
                    (:sera :serapeum))
  (:export #:image
           #:image-width #:image-height
           #:image-plane-count #:image-colormap
           #:image-pixels #:image-pixel
           #:rgb-image #:indexed-image #:grayscale-image #:planar-image #:binary-image
           #:rgb-pixel #:indexed-pixel #:grayscale-pixel #:planar-pixel

           #:imago-condition #:imago-error
           #:unknown-format #:decode-error #:not-implemented #:operation-error

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
           #:convert-to-planar #:convert-to-binary ; Aka threshold

           #:copy
           #:flip #:scale #:resize #:crop
           #:*default-interpolation*

           #:draw-point #:draw-line
           #:draw-rectangle #:draw-polygon
           #:draw-circle
           #:draw-bezier-curve

           #:convolve
           #:blur #:sharpen #:edge-detect #:emboss

           #:compose

           #:read-image #:write-image
           #:register-image-reader #:register-image-writer
           #:register-image-io-functions

           ;; Export write-jpg-stream for jupyter
           #:read-jpg #:write-jpg #:write-jpg-stream
           #:read-png #:write-png
           #:read-pnm #:write-pnm
           #:read-tga #:write-tga
           #:read-pcx #:write-pcx

           #:enhance-contrast

           ;; Algorithms for binary images
           #:label-components
           #:component-boxes
           #:erode
           #:dilate
           #:manhattan-distance-transform
           #:*cross-pattern*
           #:*square-pattern*))
