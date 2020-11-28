(in-package :imago)

(defgeneric enhance-contrast (image)
  (:documentation "Enhance the contrast of an image"))

(defun make-histogram (image)
  (let ((histogram (make-array 256 :initial-element 0 :element-type 'integer)))
    (do-image-pixels (image gray x y)
      (incf (aref histogram (gray-intensity gray))))
    histogram))

(defun histogram->cumulative-distribution-table (histogram)
  "Destructive function that mutates a histogram into a cumulative distribution table"
  (loop :for gray-level :from 1 :below (length histogram)
        :do (setf (aref histogram gray-level) (+ (aref histogram (1- gray-level))
                                                 (aref histogram gray-level))))
  histogram)

(defun cumulative-distribution-table->equalization-table (image distribution-table)
  "Destructive function that mutates a distribution table into a equalization table"
  (let ((width (image-width image))
        (height (image-height image))
        (minimum-cdf-value (loop :for i :across distribution-table :when (> (aref distribution-table i) 0) :do (return (aref distribution-table i)))))
    (loop :for i :below (length distribution-table) :do (setf (aref distribution-table i)
                                                              (round (* (/ (- (aref distribution-table i) minimum-cdf-value)
                                                                           (- (* width height) minimum-cdf-value))
                                                                      255))))
    distribution-table))


(defmethod enhance-contrast ((image grayscale-image))
  "Enhance the contrast of an IMAGE using a general histogram equalization algorithm"
  (let ((result (copy nil image))
        (equalization-table (cumulative-distribution-table->equalization-table image (histogram->cumulative-distribution-table (make-histogram image)))))
    (do-image-pixels (result gray x y)
      (setf gray (make-gray (aref equalization-table (gray-intensity gray)))))
    result))
