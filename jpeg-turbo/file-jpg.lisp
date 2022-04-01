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

(defun read-jpg-from-octets (octets)
  "Read grayscale of RGB jpeg image from a vecor of octets."
  (declare (type (simple-array (unsigned-byte 8)) octets))
  (with-decompressor (handle)
    (multiple-value-bind (width height subsamp colorspace)
        (decompress-header-from-octets handle octets)
      (declare (ignore subsamp))
      (let* ((image (make-instance (jpg-image-class colorspace)
                                   :width  width
                                   :height height))
             (data (decompress-from-octets handle octets
                              :pixel-format
                              (jpg-pixel-format image))))
        (dotimes (idx (* height width))
          (imago::read-jpg-pixel image data idx))
        image))))

(defun read-jpg (filespec)
  "Read grayscale of RGB jpeg image from a file FILESPEC."
  (declare (type (or string pathname) filespec))
  (with-open-file (input filespec :element-type '(unsigned-byte 8))
    (let ((array (make-array (file-length input)
                             :element-type '(unsigned-byte 8))))
      (read-sequence array input)
      (read-jpg-from-octets array))))

(defun write-jpg-to-octets (image
                            &key (quality 100) (subsamp nil subsamp-p))
  "Write jpeg image IMAGE to a vector of octets. QUALITY is an integer
in the range [0-100], 100 means the best quality. SUBSAMP is
subsampling mode and defaults to :S-444 for RGB images and :S-GRAY for
grayscale images."
  (declare (type (or rgb-image grayscale-image) image)
           (type (integer 1 100) quality))
  (let* ((ncomp (1- (imago::pixel-size image)))
         (width (image-width image))
         (height (image-height image))
         (length (* ncomp width height))
         (data (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (idx (* height width))
      (imago::write-jpg-pixel image data idx))
    (with-compressor (handle)
      (compress-to-octets handle data
                          width height
                          (jpg-pixel-format image)
                          :quality quality
                          :subsamp (cond
                                     (subsamp-p subsamp)
                                     ((= ncomp 1) :s-gray)
                                     (t :s-444))))))

(defun write-jpg (image filespec
                  &key (quality 100) (subsamp nil subsamp-p))
  "Write jpeg image IMAGE to a file FILESPEC. QUALITY is an integer in
the range [0-100], 100 means the best quality. SUBSAMP is subsampling
mode and defaults to :S-444 for RGB images and :S-GRAY for grayscale
images."
  (declare (type (or string pathname) filespec)
           (type (or rgb-image grayscale-image) image)
           (type (integer 1 100) quality))
  (let ((compressed (apply #'write-jpg-to-octets image
                           :quality quality
                           (if subsamp-p (list :subsamp subsamp)))))
    (with-open-file (output filespec
                            :direction :output
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (write-sequence compressed output)))
  t)

(register-image-io-functions '("jpg" "jpeg")
                             :reader #'read-jpg
                             :writer #'write-jpg)
