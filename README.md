# Imago
![CI](https://github.com/tokenrove/imago/workflows/CI/badge.svg)

Imago is an image manipulation library for Common Lisp. It supports images in
png, pcx, portable bitmap (.pnm), Truevision TGA (.tga) and jpeg formats. You
can read an image with `imago:read-image` and write an image with
`imago:write-format` where `format` is one of `png`, `pcx`, `pnm`, `tga` or
`jpg`.

## Reading from and writing to jpeg files with libjpeg-turbo

You can use more advanced [libjpeg-turbo](https://libjpeg-turbo.org/) library to
deal with jpeg files by loading `imago/jpeg-turbo` system. Make sure that
`libjpeg-turbo` is installed on your system. Use
`imago-jpeg-turbo:read-jpg-turbo` and `imago-jpeg-turbo:write-jpg-turbo`
functions (or just `imago:read-image` and `imago:write-image`) to use this
functionality.

## Reading images with pngload

You can use more advanced and faster
[pngload](https://github.com/bufferswap/pngload) library to read png files by
loading `imago/pngload` system. Use `imago-pngload:read-pngload` (or just
`imago:read-image`) to use this functionality. **NB:** `pngload` automatically
converts indexed color images to RGB (or ARGB) images. If you want to work with
indexed images, use old png loader instead.

## Usage examples

All usage examples are taken from
[here](http://matthieu.villeneuve.free.fr/dev/imago/examples.html).

### Resizing an image

`(resize *image* 400 150)`

| Original | Processed |
| -------- | --------- |
| ![Original](docs/country.png) | ![Resized](docs/country-resize.png) |


### Applying an emboss effect

`(emboss *image* :angle (* pi 0.75) :depth 1.0)`

| Original | Processed |
| -------- | --------- |
| ![Original](docs/horse.png) | ![Processed](docs/horse-emboss.png) |

### Using a custom convolution kernel

~~~~{.lisp}
(let ((kernel #2A((0  0  0  0  0)
                  (0  0  1  0  0)
                  (0  1 -4  1  0)
                  (0  0  1  0  0)
                  (0  0  0  0  0))))
  (convolve *image* kernel 1 0))
~~~~

| Original | Processed |
| -------- | --------- |
| ![Original](docs/car.png) | ![Processed](docs/car-edge-detect.png) |

### Inverting a rectangular region

~~~~
(do-region-pixels (*image* color x y 70 65 140 125)
  (setf color (invert-color color)))
~~~~

| Original | Processed |
| -------- | --------- |
| ![Original](docs/oranges.png) | ![Processed](docs/oranges-region-invert.png) |

### Adjusting contrast of a grayscale image

~~~~
(enhance-contrast *grayscale-image*)
~~~~

| Original | Processed |
| -------- | --------- |
| ![Original](docs/house.jpg) | ![Processed](docs/house-enchanced-contrast.jpg) |

### Manipulating color components

~~~~{.lisp}
(do-image-pixels (*image* color x y)
  (multiple-value-bind (r g b) (color-rgb color)
    (setf color (make-color b
                            (floor (* g 0.8))
                            r))))
~~~~

| Original | Processed |
| -------- | --------- |
| ![Original](docs/flowers.png) | ![Processed](docs/flowers-color-change.png) |

### Composing pictures

~~~~{.lisp}
(let ((operator (default-compose-operator *image1*)))
  (compose nil *image1* *image2* 20 20 operator))
~~~~

| Original 1 | Original 2 | Processed |
| ---------- | ---------- | --------- |
| ![City](docs/city.png) | ![Text](docs/text.png) | ![Composed](docs/city-text.png) |

### Drawing simple primitives

~~~~{.lisp}
(let ((points '(83 45 73 150 73 150 198 106 198 106 83 45)))
  (draw-polygon *image* points +white+ :closed t))
(draw-circle *image* 83 45 15 +white+)
(draw-circle *image* 73 150 15 +white+)
(draw-circle *image* 198 106 15 +white+)
(draw-bezier-curve *image* 10 80 150 60 100 170 200 170 +red+)
(draw-line *image* 0 5 254 5 +yellow+)
(draw-line *image* 0 10 254 10 +yellow+ :dash-length 1 :dash-interval 1)
(draw-line *image* 0 15 254 15 +yellow+ :dash-length 4 :dash-interval 2)
~~~~

| Original | Processed |
| -------- | --------- |
| ![Original](docs/fish.png) | ![Processed](docs/fish-drawing.png) |

### A more complex example

~~~~{.lisp}
(defun sea-view (image)
  (let ((image2 (flip nil image :horizontal)))
    (do-image-pixels (image2 color x y)
      (multiple-value-bind (r g b)
          (color-rgb color)
        (setf color (make-color (floor r 3) (floor g 3) (floor b 2)))))
    (let* ((width (image-width image))
           (height (image-height image))
           (result (make-instance (class-of image)
                                  :width width :height (* height 2))))
      (copy result image)
      (copy result image2 :dest-y height)
      result)))
~~~~

| Original | Processed |
| -------- | --------- |
| ![Original](docs/citynight.png) | ![Processed](docs/citynight-sea.png) |
