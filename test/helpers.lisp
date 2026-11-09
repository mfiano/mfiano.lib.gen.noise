(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.gen.noise/test
  (:local-nicknames
   (#:n #:mfiano.lib.gen.noise)
   (#:pb #:mfiano.lib.gfx.pixel-buffer))
  (:use #:mfiano.lib.base.util #:1am))

(in-package #:mfiano.lib.gen.noise/test)

(defun fill-row (field x y count)
  (let ((buffer (pb:make-buffer (1+ count))))
    (funcall field buffer 4 x y count)
    (loop :for pixel :from 1 :to count
          :collect (multiple-value-list (pb:get-channels buffer (* pixel 4))))))

(defun gray (sample)
  (let ((value (* (1+ sample) 0.5d0)))
    (list value value value 1d0)))

(defun sample-at (function sampler x y &rest z)
  (multiple-value-bind (xi xf) (floor x)
    (multiple-value-bind (yi yf) (floor y)
      (apply function sampler xi (f32 xf) yi (f32 yf) z))))

(defun simplex-2d-gradient (sampler i j)
  (let* ((unskew (/ (- 3 (sqrt 3d0)) 6))
         (x (- i (* (+ i j) unskew)))
         (y (- j (* (+ i j) unskew)))
         (gx (sample-at #'n:sample-simplex-2d sampler (+ x 1d-3) y))
         (gy (sample-at #'n:sample-simplex-2d sampler x (+ y 1d-3))))
    (values (mod (* (atan gy gx) (/ 180 pi)) 360)
            (sqrt (+ (* gx gx) (* gy gy))))))
