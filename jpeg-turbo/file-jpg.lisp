(in-package :imago-jpeg-turbo)

(defun jpg-image-class (colorspace)
  (case colorspace
    (:gray 'grayscale-image)
    ((or :cmyk :ycck)
     (error 'decode-error
            :format-control "Unsupported colorspace: ~a"
            :format-arguments (list colorspace)))
    (t 'rgb-image)))

(defgeneric jpg-pixel-format (image)
  (:documentation "Choose pixel format from reading from and writing
to jpeg images"))

(defmethod jpg-pixel-format ((image image))
  (declare (ignore image))
  (error 'decode-error
         :format-control "Unsupported image format"))

(defmethod jpg-pixel-format ((image rgb-image))
  (declare (ignore image))
  :bgr)

(defmethod jpg-pixel-format ((image grayscale-image))
  (declare (ignore image))
  :gray)

(defun read-jpg-turbo (filespec)
  "Read grayscale of RGB jpeg image from a file FILESPEC."
  (with-decompressor (handle)
    (multiple-value-bind (width height subsamp colorspace)
        (decompress-header handle filespec)
      (declare (ignore subsamp))
      (let* ((image (make-instance (jpg-image-class colorspace)
                                   :width  width
                                   :height height))
             (data (decompress handle filespec
                              :pixel-format
                              (jpg-pixel-format image))))
        (dotimes (y height)
          (dotimes (x width)
            (imago::read-jpg-pixel image data x y)))
        image))))

(defun write-jpg-turbo (image filespec
                        &key (quality 100)
                             (subsamp :s-444))
  "Write jpeg image IMAGE to a file FILESPEC. QUALITY is an integer in
the range [0-100], 100 means the best quality. SUBSAMP is subsampling
mode and defaults to :S-444 for RGB images and :S-GRAY for grayscale
images."
  (let* ((ncomp (1- (imago::pixel-size image)))
         (width (image-width image))
         (height (image-height image))
         (length (* ncomp width height))
         (data (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (dotimes (x width)
        (imago::write-jpg-pixel image data x y)))
    (with-compressor (handle)
      (compress handle filespec data
                width height
                (jpg-pixel-format image)
                :quality quality
                :subsamp subsamp)))
  t)

(register-image-io-functions '("jpg" "jpeg")
                             :reader #'read-jpg-turbo
                             :writer #'write-jpg-turbo)
