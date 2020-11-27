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
           '(read-write conversions processing))))

(def-suite read-write  :description "Image reading and writing")
(def-suite conversions :description "Conversions to different color spaces")
(def-suite processing  :description "Various image processing functions")

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
