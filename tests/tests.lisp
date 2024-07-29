(in-package :imago-tests)

(defparameter *rgb-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/batou.jpeg"))

(defparameter *grayscale-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/batou-gray.jpeg"))

(defparameter *indexed-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/parrot-indexed.png"))

(defparameter *binary-bitmap-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/batou-bitmap.pnm"))

(defparameter *ascii-bitmap-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/batou-bitmap-ascii.pnm"))

(defparameter *spheres-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/spheres.png"))

(defparameter *spheres-connected-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/spheres-connected.png"))

(defun resize-image-pathname (n)
  (asdf:system-relative-pathname
   :imago/tests (format nil "tests/test-resize~D.png" n)))

(defun temporary-filename (format)
  (asdf:system-relative-pathname
   :imago/tests
   (make-pathname :directory '(:relative "tests")
                  :name "tmp"
                  :type format)))

(defun run-tests ()
  (flet ((run-suite (suite)
           (explain! (run suite))))           
    (every #'run-suite
           '(read-write
             conversions
             processing
             contrast-enhancement
             binary-images))))

(def-suite read-write  :description "Image reading and writing")
(def-suite conversions :description "Conversions to different color spaces")
(def-suite processing  :description "Various image processing functions")
(def-suite contrast-enhancement :description "Functions to enhance contrast of images")
(def-suite binary-images :description "Algorithms for binary images")

(in-suite read-write)
(defun test-read-write (image format lossless-p)
  (let ((tmp-name (temporary-filename format))
        (width  (image-width image))
        (height (image-height image))
        (pixels (image-pixels image)))
    (finishes (write-image image tmp-name))
    (let ((image (read-image tmp-name)))
      (is (= width  (image-width image)))
      (is (= height (image-height image)))
      (when lossless-p
        (is-true (equalp (image-pixels image) pixels))))
    (delete-file tmp-name)))

(test read-write-rgb
  (let ((image (read-image *rgb-image-pathname*)))
    (mapc (alexandria:curry #'test-read-write image)
          ;; PCX is broken
          '("png" "jpg" "pnm" "tga")
          '(t nil t t))))

(test read-write-grayscale
  (let ((image (read-image *grayscale-image-pathname*)))
    (mapc (alexandria:curry #'test-read-write image)
          ;; PCX is broken, TGA is not supported
          '("png" "jpg" "pnm")
          '(t nil t))))

(test read-write-bitmap
  (let ((image (read-image *ascii-bitmap-image-pathname*)))
    (mapc (alexandria:curry #'test-read-write image)
          '("pnm") '(t))))

(test binary-vs-ascii-bitmaps
  (let ((image-binary (read-image *binary-bitmap-image-pathname*))
        (image-ascii  (read-image *ascii-bitmap-image-pathname*)))
    (is-true (equalp (image-pixels image-binary)
                     (image-pixels image-ascii)))))

;; Broken
#+nil
(test read-write-indexed
  (let ((image (read-image *indexed-image-pathname*)))
    (mapc (alexandria:curry #'test-read-write image)
          '("pnm" "pcx" "jpg" "tga" "png")
          '(t t nil t t))))


(in-suite conversions)
(defun test-converters (image conversions)
  (mapc (lambda (conversion)
          (destructuring-bind (converter . result)
              conversion
            (is (typep (funcall converter image) result))))
        conversions))

(test color-range-conversion
  (loop for bits from 1 to 8 do
        (is (= (convert-color-to-imago-format 0 bits) 0))
        (is (= (convert-color-to-imago-format (1- (ash 1 bits)) bits) 255))))

(test convert-from-rgb
  (test-converters
   (read-image *rgb-image-pathname*)
   `((,#'convert-to-rgb       . rgb-image)
     (,#'convert-to-grayscale . grayscale-image)
     #+nil ; Broken
     (,#'convert-to-indexed   . indexed-image))))

(test convert-from-grayscale
  (test-converters
   (read-image *grayscale-image-pathname*)
   `((,#'convert-to-rgb       . rgb-image)
     (,#'convert-to-grayscale . grayscale-image)
     (,#'convert-to-indexed   . indexed-image))))

(test convert-from-indexed
  ;; pngload automatically converts indexed images to RGB images
  (when (not (find-package :imago-pngio))
    (test-converters
     (read-image *indexed-image-pathname*)
     `(#+nil ; Broken
       (,#'convert-to-rgb       . rgb-image)
       #+nil ; Broken
       (,#'convert-to-grayscale . grayscale-image)
       (,#'convert-to-indexed   . indexed-image)))))


(in-suite processing)
(test resize
  (flet ((test-resize (filename)
           (let ((image (resize (read-image filename) 2000 1000)))
             (is (= (image-width  image) 2000))
             (is (= (image-height image) 1000)))))
    (mapc #'test-resize
          (list *rgb-image-pathname*
                *grayscale-image-pathname*
                *indexed-image-pathname*))))

(test resize-modes
  (flet ((verify (image1 image2)
           (let* ((pixels1 (image-pixels image1))
                  (pixels2 (image-pixels image2))
                  (dims1 (array-dimensions pixels1))
                  (dims2 (array-dimensions pixels2)))
             (is (equal dims1 dims2))
             (is (every #'=
                        (aops:flatten pixels1)
                        (aops:flatten pixels2))))))
    (let ((image1 (read-image (resize-image-pathname 1))))
      (let ((actual (resize image1 200 200 :interpolation :bicubic))
            (image2 (read-image (resize-image-pathname 2))))
        (verify actual image2))
      (let ((actual (resize image1 200 200 :interpolation :nearest-neighbor))
            (image3 (read-image (resize-image-pathname 3))))
        (verify actual image3)
        (let ((actual (resize image3 2000 2000 :interpolation :bicubic))
              (image4 (read-image (resize-image-pathname 4))))
          (verify actual image4))))))

(test convolution
  (flet ((test-convolution (filename)
           (let ((image (read-image filename)))
             (finishes (blur image))
             (finishes (sharpen image))
             (finishes (edge-detect image))
             (finishes (emboss image)))))
    (mapc #'test-convolution
          (list *rgb-image-pathname*
                *grayscale-image-pathname*))))

(in-suite contrast-enhancement)
(test enhance-contrast-of-grayscale-image
  (let* ((grays #2A((52 55 61 59 79 61 76 61)  ;; 8-bit grayscale image
                    (62 59 55 104 94 85 59 71)
                    (63 65 66 113 144 104 63 72)
                    (64 70 70 126 154 109 71 69)
                    (67 73 68 106 122 88 68 68)
                    (68 79 60 70 77 66 58 75)
                    (69 85 64 58 55 61 65 83)
                    (70 87 69 68 65 73 78 90)))
         (enhanced-grays #2A((0 12 53 32 190 53 174 53)
                             (57 32 12 227 219 202 32 154)
                             (65 85 93 239 251 227 65 158)
                             (73 146 146 247 255 235 154 130)
                             (97 166 117 231 243 210 117 117)
                             (117 190 36 146 178 93 20 170)
                             (130 202 73 20 12 53 85 194)
                             (146 206 130 117 85 166 182 215)))
         (image (make-instance 'grayscale-image
                              :pixels (aops:vectorize* 'grayscale-pixel
                                          (grays) (make-gray grays))))
         (enhanced-result (image-pixels (enhance-contrast image))))
    (is-true
     (every #'identity
            (aops:flatten
             (aops:vectorize (enhanced-grays enhanced-result)
               (= (gray-intensity enhanced-result) enhanced-grays)))))))

(in-suite binary-images)
(test label-components
  (let* ((image (read-image *spheres-image-pathname*))
         (components (label-components (convert-to-binary image 10))))
    ;; Count number of spheres
    (is (= (reduce #'max (aops:flatten components)) 10))))

(test label-components+erosion
  (let* ((image (read-image *spheres-connected-image-pathname*))
         (components (label-components (erode (convert-to-binary image 10)))))
    ;; Count number of spheres
    (is (= (reduce #'max (aops:flatten components)) 10))))

(test label-components+dilation
  (let* ((image (read-image *spheres-connected-image-pathname*))
         (components (label-components (dilate (convert-to-binary image 10)))))
    ;; Count number of spheres
    (is (= (reduce #'max (aops:flatten components)) 1))))

(defun norm-edt (i j)
  (+ (expt i 2)
     (expt j 2)))

(defun norm-mdt (i j)
  (+ (abs i) (abs j)))

(defun norm-cdt (i j)
  (max (abs i) (abs j)))

;; TODO: Randomize
(test distance-transform-with-two-feature-pixels
  (let ((image (make-instance 'binary-image
                              :width  5001
                              :height 5001))
        indices)
    (do-image-pixels (image color x y)
      (push (cons y x) indices))

    (setf (image-pixel image    0    0) 1
          (image-pixel image 5000 5000) 1)

    (loop
       for type in '(:mdt :edt :cdt)
       for norm in (list #'norm-mdt #'norm-edt #'norm-cdt)
       for dt = (distance-transform image :type type)
       do
         (is-true
          (every
           (lambda (index)
             (destructuring-bind (i . j)
                 index
               (= (aref dt i j)
                  (min (funcall norm i j)
                       (funcall norm (- 5000 i) (- 5000 j))))))
           indices)))))
