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

(defparameter *spheres-image-pathname*
  (asdf:system-relative-pathname
   :imago/tests "tests/spheres.png"))

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
(defun test-read-write (image format)
  (let ((tmp-name (temporary-filename format))
        (width (image-width image))
        (height (image-height image)))
    (finishes (write-image image tmp-name))
    (let ((image (read-image tmp-name)))
      (is (= width  (image-width image)))
      (is (= height (image-height image))))
    (delete-file tmp-name)))

(test read-write-rgb
  (let ((image (read-image *rgb-image-pathname*)))
    (mapc (lambda (format) (test-read-write image format))
          ;; PCX is broken
          '("png" "jpg" "pnm" "tga"))))

(test read-write-grayscale
  (let ((image (read-image *grayscale-image-pathname*)))
    (mapc (lambda (format) (test-read-write image format))
          ;; PCX is broken, TGA is not supported
          '("png" "jpg" "pnm"))))

;; Broken
#+nil
(test read-write-indexed
  (let ((image (read-image *indexed-image-pathname*)))
    (mapc (lambda (format) (test-read-write image format))
          '("pnm" "pcx" "jpg" "tga" "png"))))


(in-suite conversions)
(defun test-converters (image conversions)
  (mapc (lambda (conversion)
          (destructuring-bind (converter . result)
              conversion
            (is (typep (funcall converter image) result))))
        conversions))

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
  (when (not (find-package :imago-pngload))
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
  (let ((grays #2A((52 55 61 59 79 61 76 61)  ;; 8-bit grayscale image
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
        (image (make-instance 'grayscale-image :width 8 :height 8)))

    (do-image-pixels (image gray x y)
      (setf gray (make-gray (aref grays x y))))

    (do-image-pixels ((enhance-contrast image) gray x y)
      (is (= (gray-intensity gray) (aref enhanced-grays x y))))))

(in-suite binary-images)
(test label-components
  (let* ((image (read-image *spheres-image-pathname*))
         (components (label-components (convert-to-binary image 1))))
    ;; Count number os spheres
    (is (= (reduce #'max (aops:flatten components)) 10))))
