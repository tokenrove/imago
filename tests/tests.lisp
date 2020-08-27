(in-package :imago-tests)

(defparameter *input-filename*
  (asdf:system-relative-pathname
   :imago/tests "tests/batou.jpeg"))

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
           '(read-write))))

(def-suite read-write :description "Image reading and writing")
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
  (let ((image (read-image *input-filename*)))
    (mapc (lambda (format) (test-read-write image format))
          ;; PCX is broken
          '("png" "jpg" "pnm" "tga"))))

(test read-write-grayscale
  (let ((image (convert-to-grayscale (read-image *input-filename*))))
    (mapc (lambda (format) (test-read-write image format))
          ;; PCX is broken, TGA is not supported
          '("png" "jpg" "pnm"))))

(test read-write-indexed
  (let ((image (convert-to-indexed
                (convert-to-grayscale (read-image *input-filename*)))))
    (mapc (lambda (format) (test-read-write image format))
          ;; Only pnm is working
          '("pnm"))))
