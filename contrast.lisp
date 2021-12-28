(in-package :imago)

(defgeneric enhance-contrast (image)
  (:documentation "Enhance the contrast of an image"))

(deftype histogram () '(simple-array alex:non-negative-fixnum))

(defmacro do-image-pixels* ((pixel pixels) &body body)
  (let ((i (gensym)))
    `(loop for ,i below (array-total-size ,pixels) do
          (symbol-macrolet ((,pixel (row-major-aref ,pixels ,i)))
            ,@body))))

(defun make-histogram (image)
  (declare (optimize (speed 3)))
  (let ((pixels (image-pixels image))
        (histogram (make-array 256
                               :initial-element 0
                               :element-type 'alex:non-negative-fixnum)))
    (declare (type histogram histogram)
             (type (simple-array grayscale-pixel (* *)) pixels))
    (do-image-pixels* (pixel pixels)
      (incf (aref histogram (gray-intensity pixel))))
    histogram))

(defun histogram->cdt (histogram)
  "Destructive function that mutates a histogram into a cumulative distribution table"
  (declare (optimize (speed 3))
           (type histogram histogram))
  (loop
     :for gray-level :from 1 :below (length histogram)
     :do (setf (aref histogram gray-level)
               (+ (aref histogram (1- gray-level))
                  (aref histogram gray-level))))
  histogram)

(defun cdt->equalization-table (distribution-table)
  "Destructive function that mutates a distribution table into a equalization table"
  (declare (optimize (speed 3))
           (type histogram distribution-table))
  (let ((minimum-cdf-value
         (flet ((positivep (x) (> x 0)))
           (or (find-if #'positivep distribution-table) 0)))
        (pixel-count (aref distribution-table
                           (1- (length distribution-table)))))
    (map-into
     distribution-table
     (lambda (cdf-value)
       (if (zerop cdf-value) cdf-value
           (round (* (/ (- cdf-value minimum-cdf-value)
                        (- pixel-count minimum-cdf-value))
                     255))))
     distribution-table)))


(defmethod enhance-contrast ((image grayscale-image))
  "Enhance the contrast of an IMAGE using a general histogram equalization algorithm"
  (let* ((pixels (alexandria:copy-array
                  (image-pixels image)))
         (equalization-table
          (cdt->equalization-table
           (histogram->cdt
            (make-histogram image)))))
    (do-image-pixels* (pixel pixels)
      (setf pixel
            (make-gray
             (aref equalization-table
                   (gray-intensity pixel)))))
    (make-instance 'grayscale-image :pixels pixels)))
