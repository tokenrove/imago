(in-package :imago/libtiff)

;; Writing

(serapeum:-> write-rgb ((simple-array imago:rgb-pixel (* *))
                        (or pathname string))
             (values &optional))
(defun write-rgb (pixels name)
  (let ((height (array-dimension pixels 0))
        (width  (array-dimension pixels 1)))
    (cl-libtiff:with-open-tiff (tiff name :output)
      (setf (cl-libtiff:height tiff)            height
            (cl-libtiff:width tiff)             width
            ;; Interpret color information as RGB
            (cl-libtiff:photometric tiff)       :rgb
            ;; 8 bits per sample (channel)
            (cl-libtiff:bits-per-sample tiff)   8
            ;; 3 planes per pixel
            (cl-libtiff:samples-per-pixel tiff) 3
            ;; Store channel data in contignous manner
            (cl-libtiff:planar-config tiff)     :contig)
      (let ((row (make-array (cl-libtiff:scanline-size tiff)
                             :element-type '(unsigned-byte 8))))
        (assert (= (length row) (* width 3)))
        (loop for i below height
              for idx = (array-row-major-index pixels i 0) do
                (loop for j below width
                      for k from 0 by 3
                      for c = (row-major-aref pixels (+ idx j)) do
                        (setf (aref row (+ k 0))
                              (imago:color-red   c)
                              (aref row (+ k 1))
                              (imago:color-green c)
                              (aref row (+ k 2))
                              (imago:color-blue  c)))
                (cl-libtiff:write-scanline tiff row i 0)))))
  (values))

(serapeum:-> write-gray ((simple-array imago:grayscale-pixel (* *))
                        (or pathname string))
             (values &optional))
(defun write-gray (pixels name)
  (let ((height (array-dimension pixels 0))
        (width  (array-dimension pixels 1)))
    (cl-libtiff:with-open-tiff (tiff name :output)
      (setf (cl-libtiff:height tiff)            height
            (cl-libtiff:width tiff)             width
            ;; Interpret color information as min-is-black
            (cl-libtiff:photometric tiff)       :min-is-black
            ;; 8 bits per sample (channel)
            (cl-libtiff:bits-per-sample tiff)   8
            ;; 1 planes per pixel
            (cl-libtiff:samples-per-pixel tiff) 1)
      (let ((row (make-array (cl-libtiff:scanline-size tiff)
                             :element-type '(unsigned-byte 8))))
        (assert (= (length row) width))
        (loop for i below height
              for idx = (array-row-major-index pixels i 0) do
                (loop for j below width
                      for c = (row-major-aref pixels (+ idx j)) do
                        (setf (aref row j)
                              (imago:gray-intensity c)))
                (cl-libtiff:write-scanline tiff row i 0)))))
  (values))

(serapeum:-> write-tiff ((or imago:rgb-image imago:grayscale-image)
                         (or pathname string))
             (values &optional))
(defun write-tiff (image name)
  "Write RGB or grayscale image to a file NAME"
  (declare (optimize (speed 3)))
  (let ((pixels (imago:image-pixels image)))
    ;; Must be typecase here
    (etypecase image
      (imago:rgb-image
       (write-rgb pixels name))
      (imago:grayscale-image
       (write-gray pixels name)))))

;; Reading
(serapeum:-> read-tiff ((or pathname string))
             (values (or imago:rgb-image imago:grayscale-image) &optional))
(defun read-tiff (name)
  (cl-libtiff:with-open-tiff (tiff name :input)
    (let* ((width  (cl-libtiff:width       tiff))
           (height (cl-libtiff:height      tiff))
           (pm     (cl-libtiff:photometric tiff))
           (pixels
             (cl-libtiff:read-rgba-image-oriented tiff width height :topleft))
           (image (imago:make-rgb-image-from-pixels pixels)))
      (if (or (eq pm :min-is-black)
              (eq pm :max-is-black))
          (imago:convert-to-grayscale image)
          image))))

(imago:register-image-io-functions
 '("tif" "tiff")
 :reader #'read-tiff
 :writer #'write-tiff)
