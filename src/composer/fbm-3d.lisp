(in-package #:mfiano.lib.gen.noise)

(defstruct (fbm-3d
            (:include fractal-3d)
            (:constructor %make-fbm-3d)
            (:predicate nil)
            (:copier nil)))

(defmethod make-fractal-scalar ((sampler fbm-3d))
  (%make-fractal-scalar sampler))

(defun make-fbm-3d (source &key (seed 0) (octaves 4) (frequency 1) (lacunarity 2) (persistence 0.5))
  (check-type seed u64)
  (check-type source function)
  (check-type octaves fractal-octaves)
  (check-type frequency real)
  (check-type lacunarity real)
  (check-type persistence real)
  (let ((sampler (%make-fbm-3d :source-func source
                               :octaves octaves
                               :frequency (f32 frequency)
                               :lacunarity (f32 lacunarity)
                               :persistence (f32 persistence))))
    (setf (seed sampler) seed)
    sampler))

(define-ftype sample-fbm-3d (fbm-3d fixnum f32 fixnum f32 fixnum f32) f32)
(defun sample-fbm-3d (sampler xi xf yi yf zi zf)
  (declare (optimize (speed 3)))
  (let* ((func (fractal-3d-source-sample-func sampler))
         (offsets (fractal-3d-offsets sampler))
         (frequency (fractal-3d-frequency sampler))
         (lacunarity (fractal-3d-lacunarity sampler))
         (persistence (fractal-3d-persistence sampler))
         (scale (fractal-3d-scale sampler))
         (x-offset (aref offsets 0))
         (y-offset (aref offsets 1))
         (z-offset (aref offsets 2))
         (x-integer xi)
         (y-integer yi)
         (z-integer zi)
         (x-fraction (f64 xf))
         (y-fraction (f64 yf))
         (z-fraction (f64 zf)))
    (declare (type fixnum x-integer y-integer z-integer)
             (type f64 x-fraction y-fraction z-fraction))
    (loop :for source :across (fractal-3d-sources sampler)
          :for factor :of-type f32 := frequency :then lacunarity
          :for x-shift :of-type f32 := 0f0 :then x-offset
          :for y-shift :of-type f32 := 0f0 :then y-offset
          :for z-shift :of-type f32 := 0f0 :then z-offset
          :for amplitude :of-type f32 := 1f0 :then (* amplitude persistence)
          :for sample := (sample-octave func
                                        source
                                        factor
                                        (x-integer x-fraction x-shift)
                                        (y-integer y-fraction y-shift)
                                        (z-integer z-fraction z-shift))
          :sum (* sample amplitude) :into result
          :finally (return (* result scale)))))

(defmethod sample-function ((sampler fbm-3d))
  #'sample-fbm-3d)
