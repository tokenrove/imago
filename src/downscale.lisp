;;; IMAGO library
;;; Image operations
;;;
;;; Copyright (C) 2022-2023  Vasily Postnicov (shamaz.mazum@gmail.com)
;;; Copyright (C) 2004-2005  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :imago)

(sera:-> pixels-in-box ((single-float 0.0)
                        alex:non-negative-fixnum)
         (values alex:non-negative-fixnum &optional))
(defun pixels-in-box (ratio idx)
  (declare (optimize (speed 3)))
  (- (ceiling (* (+ idx 1) ratio))
     (ceiling (* (+ idx 0) ratio))))

(sera:-> downscale-channel ((simple-array (unsigned-byte 8) (* *))
                         alex:positive-fixnum alex:positive-fixnum)
         (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun downscale-channel (channel width height)
  (declare (optimize (speed 3)))
  (let ((accum (make-array (list height width)
                           :element-type    'fixnum
                           :initial-element 0))
        (new-channel (make-array (list height width)
                                 :element-type '(unsigned-byte 8))))
    (destructuring-bind (old-height old-width)
        (array-dimensions channel)
      (declare (type alex:positive-fixnum old-height old-width))
      (let ((ratio-y (/ (float old-height) height))
            (ratio-x (/ (float old-width)  width)))
        (loop for h fixnum below old-height do
              (loop for w fixnum below old-width do
                    (let ((h-new (floor (/ h ratio-y)))
                          (w-new (floor (/ w ratio-x))))
                      (incf (aref accum h-new w-new)
                            (aref channel h w)))))
        (loop for h fixnum below height do
              (loop for w fixnum below width do
                    (let ((sy (pixels-in-box ratio-y h))
                          (sx (pixels-in-box ratio-x w)))
                      (setf (aref new-channel h w)
                            (floor (aref accum h w)
                                   (* sx sy))))))))
    new-channel))

(declaim (inline downscale-pixels))
(defun downscale-pixels (pixels accessors constructor width height)
  (let ((new-pixels (make-array (list height width)
                                :element-type (array-element-type pixels)))
        (channels (mapcar
                   (lambda (accessor)
                     (declare (type (sera:-> (t) (values (unsigned-byte 8) &optional)) accessor))
                     (aops:flatten
                      (downscale-channel
                       (aops:vectorize* '(unsigned-byte 8)
                           (pixels)
                         (funcall accessor pixels))
                       width height)))
                   accessors)))
    (apply #'map-into (aops:flatten new-pixels)
           constructor channels)
    new-pixels))

(defgeneric downscale (image width heigth)
  (:documentation "Downscale an image using box sampling. This
function provides better results than RESIZE, but is slower."))

(macrolet ((define-downscale-method (image-type image-cons pixel-type pixel-cons pixel-accessors)
             `(defmethod downscale ((image ,image-type) width height)
                (declare (optimize (speed 3)))
                (let ((pixels (image-pixels image)))
                  (declare (type (simple-array ,pixel-type (* *)) pixels))
                  (,image-cons
                   (downscale-pixels
                    pixels
                    (list ,@(loop for accessor in pixel-accessors collect `#',accessor))
                    #',pixel-cons
                    width height))))))
  (define-downscale-method rgb-image make-rgb-image-from-pixels rgb-pixel make-color
                           (color-red color-green color-blue color-alpha))
  (define-downscale-method grayscale-image make-grayscale-image-from-pixels grayscale-pixel make-gray
                           (gray-intensity gray-alpha)))

(defmethod downscale :before ((image image) width height)
  (unless (and (< width  (image-width  image))
               (< height (image-height image)))
    (error 'operation-error :format-control "DOWNSCALE is used only for downscaling")))
