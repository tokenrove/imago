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
           '(read-write conversions))))

(def-suite read-write :description "Image reading and writing")
(def-suite conversions :description "Conversions to different color spaces")

(in-suite read-write)
(defun test-read-write (image format)
  (let ((tmp-name (temporary-filename format))
        (width (image-width image))
        (height (image-height image)))
    (finishes (write-image image tmp-name))
               (let ((image (read-image tmp-name)))
                 (is (= width  (image-width image)))
                 (is (= height (image-height image))))))

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
     (,#'convert-to-grayscale . grayscale-image))))

(test convert-from-grayscale
  (test-converters
   (read-image *grayscale-image-pathname*)
   `((,#'convert-to-rgb       . rgb-image)
     (,#'convert-to-grayscale . grayscale-image)
     (,#'convert-to-indexed   . indexed-image))))

(test convert-from-indexed
  (test-converters
   (read-image *indexed-image-pathname*)
   `(#+nil (,#'convert-to-rgb       . rgb-image)       ; Broken
     #+nil (,#'convert-to-grayscale . grayscale-image) ; Broken
     (,#'convert-to-indexed   . indexed-image))))
