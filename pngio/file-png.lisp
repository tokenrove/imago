(in-package :imago-pngio)

(defgeneric translate-to-imago-format (image color-type)
  (:documentation "Convert IMAGE from pngload format to imago format"))

;; KLUDGE: pngload does not signal its own conditions most of the time.
;; Re-signal decode-error if something like simple-error is signalled
(defun read-png-safely (stream)
  (handler-case
      (pngload:load-stream stream)
    ((and error (not pngload::png-error)) ()
      (error 'imago:decode-error
             :format-control "Invalid PNG stream: ~a"
             :format-arguments (list stream)))))

(defun read-png-from-stream (stream)
  (let ((image (read-png-safely stream)))
    ;; < 8 bits per color can appear in palettes
    (when (> (pngload:bit-depth image) 8)
      (error 'decode-error
             :format-control "Only 8 bits per color is supported"))
    (translate-to-imago-format
     image (pngload:color-type image))))

(imago:def-reader-from-file read-png read-png-from-stream)

(macrolet
    ((def-method (png-color-type pixel-type make-pixel make-image shape &optional color-components)
       `(defmethod translate-to-imago-format (image (color-type (eql ,png-color-type)))
          (declare (optimize (speed 3)))
          (let* ((data (pngload:data image))
                 (width (pngload:width image))
                 (height (pngload:height image))
                 (pixels (make-array (list height width)
                                     :element-type ',pixel-type))
                 (color-components (if (= (array-rank data) 2) 1
                                       (array-dimension data 2))))
            (declare (type (simple-array (unsigned-byte 8) ,shape) data)
                     (ignorable color-components))
            (loop for i fixnum below (array-total-size pixels) do
                  (setf (row-major-aref pixels i)
                        ,(if color-components
                             `(,make-pixel
                               ,@(loop for j below color-components collect
                                       `(row-major-aref data (+ (* i ,color-components) ,j))))
                             `(apply #',make-pixel
                                     (loop for j fixnum below color-components collect
                                           (row-major-aref data (+ (* i color-components) j)))))))
            (,make-image pixels)))))
  (def-method :greyscale imago:grayscale-pixel imago:make-gray
              imago:make-grayscale-image-from-pixels (* *) 1)
  (def-method :truecolour imago:rgb-pixel imago:make-color
              imago:make-rgb-image-from-pixels (* * *) 3)
  (def-method :greyscale-alpha imago:grayscale-pixel imago:make-gray
              imago:make-grayscale-image-from-pixels (* * *) 2)
  (def-method :truecolour-alpha imago:rgb-pixel imago:make-color
              imago:make-rgb-image-from-pixels (* * *) 4)
  ;; Indexed color is translated to RGB or RGBA by pngload
  (def-method :indexed-colour imago:rgb-pixel imago:make-color
              imago:make-rgb-image-from-pixels (* * *)))


(defgeneric translate-to-zpng-format (image))

;; KLUDGE: Here we always write alpha channel
(macrolet
    ((def-method (image-type color-type zpng-color-type &rest color-accessors)
       `(defmethod translate-to-zpng-format ((image ,image-type))
          (declare (optimize (speed 3)))
          (let* ((imago-pixels (imago:image-pixels image))
                 (size (array-total-size imago-pixels))
                 (colors ,(length color-accessors))
                 (zpng-pixels (make-array (* size colors) :element-type '(unsigned-byte 8))))
            (declare (type (simple-array ,color-type (* *)) imago-pixels))
            (loop for i fixnum below size
                  for pixel fixnum = (row-major-aref imago-pixels i) do
                  ,@(loop for j from 0 by 1
                          for accessor in color-accessors collect
                          `(setf (aref zpng-pixels (+ (* i colors) ,j))
                                 (,accessor pixel))))
            (make-instance 'zpng:png
                           :width  (imago:image-width  image)
                           :height (imago:image-height image)
                           :color-type ,zpng-color-type
                           :image-data zpng-pixels)))))
  (def-method imago:grayscale-image imago:grayscale-pixel :grayscale-alpha
    imago:gray-intensity imago:gray-alpha)
  (def-method imago:rgb-image imago:rgb-pixel :truecolor-alpha
    imago:color-red imago:color-green imago:color-blue imago:color-alpha))

(defun write-png-to-stream (image stream)
  "Write png image to stream using zpng library."
  (zpng:write-png-stream (translate-to-zpng-format image) stream)
  image)

(imago:def-writer-to-file write-png write-png-to-stream ())

(imago:register-image-io-functions '("png")
 :reader #'read-png
 :writer #'write-png)
