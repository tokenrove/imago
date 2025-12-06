(in-package :imago-libheif)

;; Reading

(serapeum:-> get-image ((or string pathname))
             (values (simple-array (unsigned-byte 8) 3) &optional))
(defun get-image (filename)
  (ff:with-float-traps-masked (:overflow :invalid :divide-by-zero)
    (with-libheif (+default-init-parameters+)
      (with-context (ctx)
        (context-read-from-file! ctx filename)
        (with-primary-image-handle (handle ctx)
          (let ((preferred-colorspace (image-handle-preferred-decoding-colorspace handle)))
            (multiple-value-bind (colorspace chroma channel)
                ;; FIXME: Gimp stores grayscale images with alpha in
                ;; YUV colorspace But I suppose its possible to store
                ;; Y+alpha. In this case the alpha channel will be
                ;; lost in imago.
                (if (eq preferred-colorspace :monochrome)
                    (values :monochrome :monochrome :y)
                    (values :rgb :interleaved-rgba :interleaved))
              (with-decode-image (image handle colorspace chroma +default-decoding-options+)
                (image-plane-data image channel)))))))))

(serapeum:-> data-to-rgb-image ((simple-array (unsigned-byte 8) (* * 4)))
             (values imago:rgb-image &optional))
(defun data-to-rgb-image (colors)
  (declare (optimize (speed 3)))
  (let* ((height (array-dimension colors 0))
         (width  (array-dimension colors 1))
         (pixels (make-array (list height width) :element-type 'imago:rgb-pixel)))
    (loop for i below height do
          (loop for j below width do
                (setf (aref pixels i j)
                      (imago:make-color
                       (aref colors i j 0)
                       (aref colors i j 1)
                       (aref colors i j 2)
                       (aref colors i j 3)))))
    (imago:make-rgb-image-from-pixels pixels)))

(serapeum:-> data-to-gray-image ((simple-array (unsigned-byte 8) (* * 1)))
             (values imago:grayscale-image &optional))
(defun data-to-gray-image (gray)
  (declare (optimize (speed 3)))
  (let* ((height (array-dimension gray 0))
         (width  (array-dimension gray 1))
         (pixels (make-array (list height width) :element-type 'imago:grayscale-pixel)))
    (loop for i below height do
          (loop for j below width do
                (setf (aref pixels i j)
                      (imago:make-gray
                       (aref gray i j 0)))))
    (imago:make-grayscale-image-from-pixels pixels)))

(declaim (inline data-to-image))
(defun data-to-image (colors)
  (if (= (array-dimension colors 2) 4)
      (data-to-rgb-image  colors)
      (data-to-gray-image colors)))

(serapeum:-> read-heic ((or pathname string))
             (values imago:image &optional))
(defun read-heic (filename)
  (data-to-image (get-image filename)))

;; Writing

;; KLUDGE: There is no other way
(serapeum:-> has-alpha-p ((simple-array imago:grayscale-pixel 2))
             (values boolean &optional))
(defun has-alpha-p (pixels)
  (declare (optimize (speed 3)))
  (loop for i below (array-total-size pixels)
        when (/= (imago:gray-alpha (row-major-aref pixels i)) 255) do
        (return t) finally (return nil)))

(serapeum:-> gray-to-data ((simple-array imago:grayscale-pixel 2))
             (values (simple-array (unsigned-byte 8) (* * 1)) &optional))
(defun gray-to-data (pixels)
  (declare (optimize (speed 3)))
  (let* ((height (array-dimension pixels 0))
         (width  (array-dimension pixels 1))
         (result (make-array (list height width 1)
                             :element-type '(unsigned-byte 8))))
    (loop for i below height do
          (loop for j below width do
                (setf (aref result i j 0)
                      (imago:gray-intensity
                       (aref pixels i j)))))
    result))

(serapeum:-> color-to-data ((simple-array imago:rgb-pixel 2))
             (values (simple-array (unsigned-byte 8) (* * 4)) &optional))
(defun color-to-data (pixels)
  (declare (optimize (speed 3)))
  (let* ((height (array-dimension pixels 0))
         (width  (array-dimension pixels 1))
         (result (make-array (list height width 4)
                             :element-type '(unsigned-byte 8))))
    (loop for i below height do
          (loop for j below width do
                (let ((color (aref pixels i j)))
                  (setf (aref result i j 0)
                        (imago:color-red color)
                        (aref result i j 1)
                        (imago:color-green color)
                        (aref result i j 2)
                        (imago:color-blue color)
                        (aref result i j 3)
                        (imago:color-alpha color)))))
    result))

(serapeum:-> image-to-data (imago:image)
             (values (simple-array (unsigned-byte 8) 3) &optional))
(defun image-to-data (image)
  (let ((pixels (imago:image-pixels image)))
    (cond
      ((typep image 'imago:rgb-image)
       (color-to-data pixels))
      ((and (typep image 'imago:grayscale-image)
            (has-alpha-p pixels))
       (image-to-data
        (imago:convert-to-rgb image)))
      ((typep image 'imago:grayscale-image)
       (gray-to-data pixels))
      (t (error 'encode-error
                :format-control "Image of type ~a is not supported"
                :format-arguments (list (type-of image)))))))

(serapeum:-> write-heic
             (imago:image (or pathname string) &optional compression-format (integer 0 100))
             (values &optional))
(defun write-heic (image filename &optional (format :hevc) (quality 80))
  (let ((data (image-to-data image))
        (width  (imago:image-width image))
        (height (imago:image-height image)))
    (ff:with-float-traps-masked (:overflow :invalid :divide-by-zero)
      (with-libheif (+default-init-parameters+)
        (multiple-value-bind (colorspace chroma channel)
            (if (= (array-dimension data 2) 4)
                (values :rgb :interleaved-rgba :interleaved)
                (values :monochrome :monochrome :y))
          (with-image (img width height colorspace chroma)
            (image-add-plane! img channel width height 8)
            (image-set-plane-data! img channel data)
            (with-context (ctx)
              (with-encoder-for-format (encoder ctx format)
                (encoder-set-lossy-quality! encoder quality)
                (context-encode-image! ctx img encoder +default-encoding-options+))
              (context-write-to-file! ctx filename)))))))
  (values))

(imago:register-image-io-functions
 '("heic")
 :reader #'read-heic
 :writer #'write-heic)
