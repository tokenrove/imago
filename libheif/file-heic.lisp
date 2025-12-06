(in-package :imago-libheif)

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

(defun data-to-image (colors)
  (if (= (array-dimension colors 2) 4)
      (data-to-rgb-image  colors)
      (data-to-gray-image colors)))

(defun read-heic (filename)
  (data-to-image (get-image filename)))
