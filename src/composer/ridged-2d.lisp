(in-package #:mfiano.lib.gen.noise)

(defstruct (ridged-2d
            (:include fractal-2d)
            (:constructor %make-ridged-2d)
            (:predicate nil)
            (:copier nil))
  (attenuation 2f0 :type f32))

(defmethod fractal-attenuation ((sampler ridged-2d))
  (ridged-2d-attenuation sampler))

(defmethod make-fractal-scalar ((sampler ridged-2d))
  (%make-fractal-scalar/ridged sampler))

(defun make-ridged-2d (source
                       &key (seed 0) (octaves 4) (frequency 1) (lacunarity 2)
                         (persistence 1.0) (attenuation 2.0))
  (let ((sampler (%make-ridged-2d :source-func source
                                  :octaves octaves
                                  :frequency (f32 frequency)
                                  :lacunarity (f32 lacunarity)
                                  :persistence (f32 persistence)
                                  :attenuation (f32 attenuation))))
    (setf (seed sampler) seed)
    sampler))

(define-ftype sample-ridged-2d (t fixnum f32 fixnum f32) f32)
(defun sample-ridged-2d (sampler xi xf yi yf)
  (declare (optimize (speed 3)))
  (let* ((func (fractal-2d-source-sample-func sampler))
         (offsets (fractal-2d-offsets sampler))
         (frequency (fractal-2d-frequency sampler))
         (lacunarity (fractal-2d-lacunarity sampler))
         (persistence (fractal-2d-persistence sampler))
         (attenuation (ridged-2d-attenuation sampler))
         (scale (fractal-2d-scale sampler))
         (x-offset (aref offsets 0))
         (y-offset (aref offsets 1)))
    (loop :for source :across (fractal-2d-sources sampler)
          :for amplitude :of-type f32 := 1f0 :then (* amplitude persistence)
          :for weight := 1f0 :then (s32:clamp (/ sample attenuation) 0f0 1f0)
          :for fx :of-type f32 := (* (+ xi xf) frequency) :then (+ (* fx lacunarity) x-offset)
          :for fy :of-type f32 := (* (+ yi yf) frequency) :then (+ (* fy lacunarity) y-offset)
          :for %sample := (multiple-value-call func source (floor fx) (floor fy))
          :for sample := (* (expt (- 1 (abs %sample)) 2) weight amplitude)
          :sum sample :into result
          :finally (return (1- (* result scale))))))

(defmethod sample-function ((sampler ridged-2d))
  #'sample-ridged-2d)
