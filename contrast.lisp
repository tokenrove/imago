(in-package :imago)

(defgeneric enhance-contrast (image)
  (:documentation "Enhance the contrast of an image"))

(defun make-histogram (image)
  (let ((histogram (make-array 256 :initial-element 0 :element-type 'integer)))
    (do-image-pixels (image gray x y)
      (incf (aref histogram (gray-intensity gray))))
    histogram))

(defun histogram->cdt (histogram)
  "Destructive function that mutates a histogram into a cumulative distribution table"
  (loop
     :for gray-level :from 1 :below (length histogram)
     :do (setf (aref histogram gray-level)
               (+ (aref histogram (1- gray-level))
                  (aref histogram gray-level))))
  histogram)

(defun cdt->equalization-table (image distribution-table)
  "Destructive function that mutates a distribution table into a equalization table"
  (let ((width (image-width image))
        (height (image-height image))
        (minimum-cdf-value
         (flet ((positivep (x) (> x 0)))
           (or (find-if #'positivep distribution-table) 0))))
    (map-into
     distribution-table
     (lambda (cdf-value)
       (if (zerop cdf-value) cdf-value
           (round (* (/ (- cdf-value minimum-cdf-value)
                        (- (* width height) minimum-cdf-value))
                     255))))
     distribution-table)))


(defmethod enhance-contrast ((image grayscale-image))
  "Enhance the contrast of an IMAGE using a general histogram equalization algorithm"
  (let ((result (copy nil image))
        (equalization-table
         (cdt->equalization-table
          image (histogram->cdt (make-histogram image)))))
    (do-image-pixels (result gray x y)
      (setf gray (make-gray (aref equalization-table (gray-intensity gray)))))
    result))
