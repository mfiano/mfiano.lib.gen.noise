(in-package #:mfiano.lib.gen.noise)

(defstruct (multi-3d
            (:include fractal-3d)
            (:constructor %make-multi-3d)
            (:predicate nil)
            (:copier nil)))

(defmethod make-fractal-scalar ((sampler multi-3d))
  (%make-fractal-scalar/multi sampler))

(defun make-multi-3d (source
                      &key (seed 0) (octaves 4) (frequency 1) (lacunarity 2) (persistence 0.5))
  (let ((sampler (%make-multi-3d :source-func source
                                 :octaves octaves
                                 :frequency (f32 frequency)
                                 :lacunarity (f32 lacunarity)
                                 :persistence (f32 persistence))))
    (setf (seed sampler) seed)
    sampler))

(define-ftype sample-multi-3d (multi-3d fixnum f32 fixnum f32 fixnum f32) f32)
(defun sample-multi-3d (sampler xi xf yi yf zi zf)
  (declare (optimize (speed 3)))
  (let* ((func (fractal-3d-source-sample-func sampler))
         (offsets (fractal-3d-offsets sampler))
         (frequency (fractal-3d-frequency sampler))
         (lacunarity (fractal-3d-lacunarity sampler))
         (persistence (fractal-3d-persistence sampler))
         (scale (fractal-3d-scale sampler))
         (x-offset (aref offsets 0))
         (y-offset (aref offsets 1))
         (z-offset (aref offsets 2)))
    (loop :for source :across (fractal-3d-sources sampler)
          :for fx :of-type f32 := (* (+ xi xf) frequency) :then (+ (* fx lacunarity) x-offset)
          :for fy :of-type f32 := (* (+ yi yf) frequency) :then (+ (* fy lacunarity) y-offset)
          :for fz :of-type f32 := (* (+ zi zf) frequency) :then (+ (* fz lacunarity) z-offset)
          :for amplitude := 1f0 :then (* amplitude persistence)
          :for sample :of-type f32 := (multiple-value-call func
                                        source
                                        (floor fx)
                                        (floor fy)
                                        (floor fz))
          :for result :of-type f32 := sample :then (+ result (* result sample amplitude))
          :finally (return (* result scale)))))

(defmethod sample-function ((sampler multi-3d))
  #'sample-multi-3d)
