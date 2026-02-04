(in-package #:mfiano.lib.gen.noise)

(defstruct (fbm-2d
            (:include fractal-2d)
            (:constructor %make-fbm-2d)
            (:predicate nil)
            (:copier nil)))

(defmethod make-fractal-scalar ((sampler fbm-2d))
  (%make-fractal-scalar sampler))

(defun make-fbm-2d (source &key (seed 0) (octaves 4) (frequency 1) (lacunarity 2) (persistence 0.5))
  (check-type seed u64)
  (check-type source function)
  (check-type octaves fractal-octaves)
  (check-type frequency real)
  (check-type lacunarity real)
  (check-type persistence real)
  (let ((sampler (%make-fbm-2d :source-func source
                               :octaves octaves
                               :frequency (f32 frequency)
                               :lacunarity (f32 lacunarity)
                               :persistence (f32 persistence))))
    (setf (seed sampler) seed)
    sampler))

(define-ftype sample-fbm-2d (fbm-2d fixnum f32 fixnum f32) f32)
(defun sample-fbm-2d (sampler xi xf yi yf)
  (declare (optimize (speed 3)))
  (let* ((func (fractal-2d-source-sample-func sampler))
         (offsets (fractal-2d-offsets sampler))
         (frequency (fractal-2d-frequency sampler))
         (lacunarity (fractal-2d-lacunarity sampler))
         (persistence (fractal-2d-persistence sampler))
         (scale (fractal-2d-scale sampler))
         (x-offset (aref offsets 0))
         (y-offset (aref offsets 1)))
    (loop :for source :across (fractal-2d-sources sampler)
          :for fx :of-type f32 := (* (+ xi xf) frequency) :then (+ (* fx lacunarity) x-offset)
          :for fy :of-type f32 := (* (+ yi yf) frequency) :then (+ (* fy lacunarity) y-offset)
          :for amplitude :of-type f32 := 1f0 :then (* amplitude persistence)
          :for sample := (multiple-value-call func source (floor fx) (floor fy))
          :sum (* sample amplitude) :into result
          :finally (return (* result scale)))))

(defmethod sample-function ((sampler fbm-2d))
  #'sample-fbm-2d)
