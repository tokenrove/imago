;;; IMAGO library
;;; PCX file handling
;;;
;;; Supports all older styles of PCX with which I'm familiar, except
;;; those designed for CGA (2bit).
;;;
;;; Copyright (C) 2005  Julian Squires <julian@cipht.net>
;;;
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package :imago)

(defun read-pcx (filespec)
  (with-open-file (stream filespec :direction :input
                   :element-type '(unsigned-byte 8))
    ;; Read header.
    (let* ((manufacturer (read-byte stream))
           (version (read-byte stream))
           (encoding (read-byte stream))
           (bits-per-pixel (read-byte stream))
           (x-min (read-lsb-integer stream 2))
           (y-min (read-lsb-integer stream 2))
           (x-max (read-lsb-integer stream 2))
           (y-max (read-lsb-integer stream 2))
           (width (1+ (- x-max x-min)))
           (height (1+ (- y-max y-min)))
           (ega-palette (progn
                          (skip-bytes stream 4) ; Skip DPI.
                          (read-byte-array stream 48)))
           (color-planes (progn (skip-bytes stream 1) ; Skip reserved.
                                (read-byte stream)))
           (bytes-per-line (read-lsb-integer stream 2)))

      ;; Skip lsb shorts representing palette type, horizontal screen
      ;; size, and vertical screen size, respectively.
      ;; XXX we shouldn't ignore palette-type, even though it's really
      ;; rarely used in my experience.
      (skip-bytes stream 6)

      ;; Skip the filler.
      (skip-bytes stream 54)
      (assert (= (file-position stream) 128))

      ;; No support for other encodings.  Our actual support for
      ;; earlier versions is rather limited; mostly we allow version
      ;; values other than 5 to avoid hassles with ghostscript pcx16
      ;; output (which has version set to 2).
      (assert (and (= manufacturer 10)
                   (member version '(0 2 3 4 5) :test #'=)
                   (= encoding 1)))

      ;; Consistency check bpp against color planes.
      (assert (member (* bits-per-pixel color-planes) '(1 4 8 24)
                      :test #'=))

      (let ((image
             (ecase (* bits-per-pixel color-planes)
               (1 (make-instance 'planar-image :width width :height height
                                 :plane-count 1))
               (4 (make-instance 'planar-image :width width :height height
                                 :plane-count 4))
               (8 (make-instance 'indexed-image :width width :height height
                                 :color-count 256))
               (24 (make-instance 'rgb-image
                                  :width width :height height)))))

        ;; Read data.
        (read-pcx-data stream image color-planes bytes-per-line)

        (case (* bits-per-pixel color-planes)
          (1 (replace (image-colormap image) (list +black+ +white+)))
          (4 (pcx-ega-palette->colormap ega-palette (image-colormap image)))
          (8 (read-pcx-vga-palette stream image)))

        image))))

;; Note that bytes per line is bytes per plane per scanline.
(defun read-pcx-data (stream image color-planes bytes-per-line)
  (flet ((rgb-helper (plane new r g b)
           (declare (ignorable r g b))
           (ecase plane
             (0 (make-color new g b))
             (1 (make-color r new b))
             (2 (make-color r g new)))))
    (let ((buffer (make-array `(,bytes-per-line)
                              :element-type 'unsigned-byte
                              :fill-pointer 0)))
      (dotimes (y (image-height image))
        (dotimes (plane color-planes)
          ;; read and un-RLE until we've filled the buffer.
          (setf (fill-pointer buffer) 0)
          (do ()
              ((= (fill-pointer buffer) bytes-per-line))
            (let ((byte (read-byte stream)))
              (cond ((/= (logand byte #xC0) #xC0) (vector-push byte buffer))
                    (t (let ((value (read-byte stream)))
                         (dotimes (n (logand byte #x3F))
                           (vector-push value buffer)))))))
          ;; unpack into suitable plane
          (etypecase image
            ;; If it's an indexed image, copy the buffer in directly.
            (indexed-image
             (do-region-pixels (image pixel x y 0 y (image-width image) 1)
               (setf pixel (aref buffer x))))
            ;; If it's an RGB image, map plane -> {R,G,B}
            (rgb-image
             (do-region-pixels (image pixel x y 0 y (image-width image) 1)
               (setf pixel (rgb-helper plane (aref buffer x)
                                       (color-red pixel)
                                       (color-green pixel)
                                       (color-blue pixel)))))
            (planar-image
             (do-region-pixels (image pixel x y 0 y (image-width image) 1)
               (multiple-value-bind (q r) (floor x 8)
                 ;; If it's a planar image, write to suitable plane.
                 (setf (ldb (byte 1 plane) pixel)
                       (ldb (byte 1 r) (aref buffer q))))))))))))

(defun read-pcx-vga-palette (stream image)
  ;; Doing it "The Z-Soft Way"(TM).  It would probably be better to
  ;; just read until we hit a byte equal to 12 or EOF -- file-position
  ;; and file-length aren't supported by all streams.
  (file-position stream (- (file-length stream) 769))
  (when (= 12 (read-byte stream nil 255))
    (dotimes (i 256)
      (setf (aref (image-colormap image) i)
            (make-color (read-byte stream)
                        (read-byte stream)
                        (read-byte stream))))))

(defun pcx-ega-palette->colormap (ega-palette colormap)
  (dotimes (i 16)
    (setf (aref colormap i)
          (make-color (aref ega-palette (* i 3))
                      (aref ega-palette (+ 1 (* i 3)))
                      (aref ega-palette (+ 2 (* i 3)))))))

(defun colormap->pcx-ega-palette (colormap ega-palette)
  (dotimes (i 16)
    (let ((color (aref colormap i)))
      (setf (aref ega-palette (* i 3)) (color-red color)
            (aref ega-palette (+ 1 (* i 3))) (color-green color)
            (aref ega-palette (+ 2 (* i 3))) (color-blue color)))))

(register-image-reader '("pcx" "PCX") #'read-pcx)


(defun write-pcx (image filespec
                  &key (max-run-length 63)
                  even-scanline-lengths-p)
  "Write the given IMAGE to a Z-Soft PCX 3.0 format file named by
FILESPEC.

MAX-RUN-LENGTH => The longest run the PCX encoder will output.  This
can be as large as 63 (the default), but to support old and broken
decoders, it should be set to 15.  I've never seen a decoder so broken
that it won't read >15 byte runs, but they do exist, so the option is
here.

EVEN-SCANLINE-LENGTHS-P => When set, will pad scanlines so that they
are an even length.  Supposedly there are decoders that want this
behavior, but it's not necessary for most decoders."
  ;; Determine whether this image can be output as a PCX.
  ;; XXX: Should add greyscale-image support, convert to paletted
  ;; image if possible?  Would this make palette-type finally useful?
  (multiple-value-bind (bits-per-pixel color-planes)
      (etypecase image
        (rgb-image (values 8 3))
        (indexed-image (values 8 1))
        (planar-image (assert (or (= (image-plane-count image) 1)
                                  (= (image-plane-count image) 4)))
                      (values 1 (image-plane-count image))))

    (with-open-file (stream filespec :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      ;; Output header.
      (write-byte 10 stream)            ; Manufacturer.
      (write-byte 5 stream)             ; Version.
      (write-byte 1 stream)             ; Encoding.
      (write-byte bits-per-pixel stream)
      (write-lsb-integer 0 stream 2)    ; X min
      (write-lsb-integer 0 stream 2)    ; Y min
      (write-lsb-integer (1- (image-width image)) stream 2) ; X max
      (write-lsb-integer (1- (image-height image)) stream 2) ; Y max
      ;; XXX DPI values.
      (write-lsb-integer 0 stream 4)
      ;; write ega-palette
      (let ((ega-palette (make-array '(48) :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
        (when (or (typep image 'indexed-image)
                  (typep image 'planar-image))
          (colormap->pcx-ega-palette (image-colormap image) ega-palette))
        (write-sequence ega-palette stream))
      (write-byte 0 stream)             ; Reserved.
      (write-byte color-planes stream)
      ;; Bytes per line -- no rowstride.
      (let ((bytes-per-line (ceiling (* bits-per-pixel (image-width image))
                                     8)))
        (when (and even-scanline-lengths-p (oddp bytes-per-line))
          (incf bytes-per-line))

        (write-lsb-integer bytes-per-line stream 2)
        ;; XXX Should palette type be something other than 1
        ;; (supposedly, color) for 1 and 4 bpp images?
        (write-lsb-integer 1 stream 2)  ; Palette type.
        (write-lsb-integer 0 stream 2)  ; Horizontal screen size.
        (write-lsb-integer 0 stream 2)  ; Vertical screen size.
        ;; Filler.
        (let ((filler (make-array '(54) :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
          (write-sequence filler stream))

        (assert (= (file-position stream) 128))

        ;; Output body.
        (write-pcx-body stream image color-planes bytes-per-line
                        max-run-length))

      ;; Output VGA palette if necessary.
      (when (typep image 'indexed-image)
        (write-byte 12 stream)
        (dotimes (i 256)
          (write-byte (color-red (aref (image-colormap image) i)) stream)
          (write-byte (color-green (aref (image-colormap image) i)) stream)
          (write-byte (color-blue (aref (image-colormap image) i)) stream)))))
  image)


;;; XXX The efficiency of this function could be decently improved by
;;; being a bit more clever about how we accumulate the bytes for each
;;; plane.
(defun write-pcx-body (stream image color-planes bytes-per-line max-run-length)
  (let ((buffer (make-array `(,bytes-per-line)
                            :element-type 'unsigned-byte
                            :initial-element 0)))
    (dotimes (scanline (image-height image))
      (dotimes (plane color-planes)
        ;; extract this plane into bytes
        (fill-pcx-scanline-buffer image scanline plane buffer)
        ;; output the bytes
        (do-runs (value i run (lambda (i) (aref buffer i)) (length buffer))
          (if (and (= run 1) (< value #xC0))
              (write-byte value stream)
              (do ((run-chunk (min run max-run-length)
                              (min run max-run-length)))
                  ((<= run 0))
                (write-byte (logior #xC0 run-chunk) stream)
                (write-byte value stream)
                (decf run run-chunk))))))))

(defun fill-pcx-scanline-buffer (image scanline plane buffer)
  (let ((i 0))
    (etypecase image
      (rgb-image
       (let ((plane-fn (ecase plane
                         (0 #'color-red)
                         (1 #'color-green)
                         (2 #'color-blue))))
         (do-region-pixels (image pixel x y 0 scanline (image-width image) 1)
           (setf (aref buffer i) (funcall plane-fn pixel))
           (incf i))))
      (indexed-image
       (do-region-pixels (image pixel x y 0 scanline (image-width image) 1)
         (setf (aref buffer i) pixel)
         (incf i)))
      (planar-image
       ;; I bet this could be a lot nicer.
       (let ((bit-counter 0)
             (bit-buffer 0))
         (do-region-pixels (image pixel x y 0 scanline (image-width image) 1)
           (setf bit-buffer (dpb  (ldb (byte 1 plane) pixel)
                                  (byte 1 (mod x 8))
                                  bit-buffer))
           (incf bit-counter)
           (when (> bit-counter 7)
             (setf (aref buffer i) bit-buffer)
             (incf i)
             (setf bit-buffer 0 bit-counter 0))))))

    ;; If the buffer isn't totally full yet (a common occurrance when
    ;; there are padding bytes at the end of a scanline), why don't we
    ;; change the ending bytes to whatever the last byte was, so we
    ;; can get a little extra out of the RLE.
    (do ((j (max (1- i) 0) (1+ j))
         (v (aref buffer (1- i))))
        ((>= j (length buffer)))
      (setf (aref buffer j) v))))

