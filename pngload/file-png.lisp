(in-package :imago-pngload)

(defgeneric translate-to-imago-format (image color-type)
  (:documentation "Convert IMAGE from pngload format to imago format"))

(defun read-pngload (pathname)
  (let ((image (pngload:load-file pathname)))
    (if (= (pngload:bit-depth image) 16)
        (error 'decode-error
               :format-control "16 bit color is not supported"))
    (translate-to-imago-format
     image (pngload:color-type image))))

(macrolet
    ((def-method (png-color-type
                  imago-pixel-type
                  make-pixel-fn
                  imago-image-type
                  color-components)
       `(defmethod translate-to-imago-format (image (color-type (eql ,png-color-type)))
          (declare (optimize (speed 3))
                   (ignore color-type))
          (let* ((data (pngload:data image))
                 (width (pngload:width image))
                 (height (pngload:height image))
                 (depth (pngload:bit-depth image))
                 (pixels (make-array (list height width)
                                     :element-type ',imago-pixel-type)))
            (declare (type (simple-array ,imago-pixel-type) pixels)
                     (type (simple-array (unsigned-byte 8)) data)
                     (type (integer 1 8) depth))
            (dotimes (y height)
              (dotimes (x width)
                (setf (aref pixels y x)
                      (,make-pixel-fn
                       ,@(loop for i below color-components collect
                              `(the fixnum
                                    (ash (aref data y x ,@(if (not (= color-components 1))
                                                              (list i)))
                                         (- 8 depth))))))))
            (make-instance ',imago-image-type :pixels pixels)))))
  (def-method :greyscale grayscale-pixel make-gray grayscale-image 1)
  (def-method :truecolour rgb-pixel make-color rgb-image 3)
  (def-method :greyscale-alpha grayscale-pixel make-gray grayscale-image 2)
  (def-method :truecolour-alpha rgb-pixel make-color rgb-image 4))

;; Indexed color is translated to RGB or RGBA by pngload
(defmethod translate-to-imago-format (image (color-type (eql :indexed-colour)))
  (declare (optimize (speed 3))
           (ignore color-type))
  (let* ((data (pngload:data image))
         (width (pngload:width image))
         (height (pngload:height image))
         (color-components (nth 2 (array-dimensions data)))
         (pixels (make-array (list height width)
                             :element-type 'rgb-pixel)))
    (declare (type (simple-array rgb-pixel) pixels)
             (type (simple-array (unsigned-byte 8)) data)
             (type (integer 3 4) color-components))
    (flet ((get-color (y x)
             (apply #'make-color
                    (loop for i below color-components collect
                         (aref data y x i)))))
      (dotimes (y height)
        (dotimes (x width)
          (setf (aref pixels y x)
                (get-color y x)))))
    (make-instance 'rgb-image :pixels pixels)))

(register-image-io-functions
 '("png")
 :reader #'read-pngload)
