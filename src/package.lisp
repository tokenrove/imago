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

(defpackage :imago
  (:use #:common-lisp)
  (:local-nicknames (:alex :alexandria)
                    (:sera :serapeum)
                    (:flex :flexi-streams)
                    (:bit  :imago-bit-io))
  (:export #:image
           #:image-width #:image-height
           #:image-plane-count #:image-colormap
           #:image-pixels #:image-pixel
           #:rgb-image #:indexed-image #:grayscale-image #:planar-image #:binary-image
           #:rgb-pixel #:indexed-pixel #:grayscale-pixel #:planar-pixel
           #:make-rgb-image #:make-rgb-image-from-pixels
           #:make-grayscale-image #:make-grayscale-image-from-pixels
           #:make-binary-image #:make-binary-image-from-pixels

           #:imago-condition #:imago-error
           #:unknown-format #:decode-error #:encode-error
           #:not-implemented #:operation-error

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
           #:flip #:scale #:resize #:downscale #:crop #:rotate
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
           #:def-reader-from-file
           #:def-writer-to-file

           ;; TODO: stream versions for all types.
           #:read-jpg #:write-jpg #:read-jpg-from-stream #:write-jpg-to-stream
           #:read-png #:write-png #:read-png-from-stream #:write-png-to-stream
           #:read-pnm #:write-pnm #:read-pnm-from-stream #:write-pnm-to-stream
           #:read-tga #:write-tga #:read-tga-from-stream #:write-tga-to-stream
           #:read-pcx #:write-pcx

           #:enhance-contrast

           ;; Algorithms for binary images
           #:label-components
           #:component-boxes
           #:erode #:dilate #:*structuring-element*
           #:thin
           #:distance-transform
           #:+cross-pattern+
           #:+square-pattern+))
