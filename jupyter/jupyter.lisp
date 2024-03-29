(in-package :imago-jupyter)

(defgeneric show-image (image)
  (:documentation "Show an image in Jupyter notebook"))

;; KLUDGE: since binary images cannot be saved, convert them to
;; grayscale first.
;; Maybe write saver functions to binary images? But since jpeg is
;; lossy format, such images will cease to be binary after
;; saving. This is tricky...

(defmethod show-image ((image image))
  (png
   (usb8-array-to-base64-string
    (flexi-streams:with-output-to-sequence (stream)
      (imago-pngio:write-png-to-stream image stream)))))

(defmethod show-image ((image binary-image))
  (show-image (convert-to-grayscale image)))
