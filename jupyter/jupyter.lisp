(in-package :imago-jupyter)

(defun show-image (image)
  (declare (type image image))
  (jpeg
   (usb8-array-to-base64-string
    (with-output-to-sequence (stream)
      (write-jpg-stream image stream)))))
