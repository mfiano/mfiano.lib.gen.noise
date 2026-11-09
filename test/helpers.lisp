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

(defun split-coordinates (coordinates)
  (loop :for coordinate :in coordinates
        :nconc (multiple-value-bind (integer fraction) (floor coordinate)
                 (list integer (f32 fraction)))))

(defun fractal-parameters (sampler)
  (if (= (n::dimensions sampler) 2)
      (values (n::fractal-2d-offsets sampler)
              (n::fractal-2d-frequency sampler)
              (n::fractal-2d-lacunarity sampler))
      (values (n::fractal-3d-offsets sampler)
              (n::fractal-3d-frequency sampler)
              (n::fractal-3d-lacunarity sampler))))

(defun hybrid-multi-reference (sampler sample &rest coordinates)
  (multiple-value-bind (offsets frequency lacunarity) (fractal-parameters sampler)
    (let ((offsets (map 'list #'rational offsets))
          (persistence (rational (n::fractal-persistence sampler))))
      (flet ((musgrave (noise)
               (loop :with points := (mapcar (lambda (coordinate)
                                               (* (rational coordinate) (rational frequency)))
                                             coordinates)
                     :with result := 0
                     :with weight := 1
                     :for source :across (n::fractal-sources sampler)
                     :for octave :from 0
                     :for amplitude := 1 :then (* amplitude persistence)
                     :while (> weight 1/1000)
                     :do (when (plusp octave)
                           (setf points (mapcar (lambda (point offset)
                                                  (+ (* point (rational lacunarity)) offset))
                                                points
                                                offsets)))
                         (let ((signal (* (+ (funcall noise source points) 7/10) amplitude)))
                           (setf weight (min weight 1)
                                 result (+ result (* weight signal))
                                 weight (* weight signal)))
                     :finally (return result))))
        (let ((maximum (musgrave (constantly 1)))
              (result (musgrave (lambda (source points)
                                  (rational (apply sample source (split-coordinates points)))))))
          (1- (/ (* (+ result 3/10) 2) (+ maximum 3/10))))))))

(defun composers ()
  (list (list #'n:make-fbm-2d #'n:sample-fbm-2d #'n:make-perlin-2d)
        (list #'n:make-billow-2d #'n:sample-billow-2d #'n:make-perlin-2d)
        (list #'n:make-multi-2d #'n:sample-multi-2d #'n:make-perlin-2d)
        (list #'n:make-hybrid-multi-2d #'n:sample-hybrid-multi-2d #'n:make-perlin-2d)
        (list #'n:make-ridged-2d #'n:sample-ridged-2d #'n:make-perlin-2d)
        (list #'n:make-fbm-3d #'n:sample-fbm-3d #'n:make-perlin-3d)
        (list #'n:make-billow-3d #'n:sample-billow-3d #'n:make-perlin-3d)
        (list #'n:make-multi-3d #'n:sample-multi-3d #'n:make-perlin-3d)
        (list #'n:make-hybrid-multi-3d #'n:sample-hybrid-multi-3d #'n:make-perlin-3d)
        (list #'n:make-ridged-3d #'n:sample-ridged-3d #'n:make-perlin-3d)))

(defun large-coordinate-samples (make sample source)
  (let ((sampler (funcall make source :seed 1)))
    (loop :for step :from 1 :to 5
          :collect (apply sample
                          sampler
                          (expt 2 24)
                          (* step 0.15f0)
                          3
                          0.5f0
                          (when (= (n::dimensions sampler) 3)
                            (list 0 0.5f0))))))

(defun scale-coordinate-error (integer fraction factor shift)
  (let ((exact (+ (* (+ integer (rational fraction)) (rational factor)) (rational shift))))
    (multiple-value-bind (scaled-integer scaled-fraction)
        (n::scale-coordinate integer fraction factor shift)
      (abs (- (+ scaled-integer (rational scaled-fraction)) exact)))))
