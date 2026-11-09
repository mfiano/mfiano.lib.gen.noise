(in-package #:mfiano.lib.gen.noise)

(defstruct (hybrid-multi-2d
            (:include fractal-2d)
            (:constructor %make-hybrid-multi-2d)
            (:predicate nil)
            (:copier nil)))

(defmethod make-fractal-scalar ((sampler hybrid-multi-2d))
  (%make-fractal-scalar/hybrid-multi sampler))

(defun make-hybrid-multi-2d (source
                             &key (seed 0) (octaves 4) (frequency 1) (lacunarity 2)
                               (persistence 0.25))
  (let ((sampler (%make-hybrid-multi-2d :source-func source
                                        :octaves octaves
                                        :frequency (f32 frequency)
                                        :lacunarity (f32 lacunarity)
                                        :persistence (f32 persistence))))
    (setf (seed sampler) seed)
    sampler))

(define-ftype sample-hybrid-multi-2d (hybrid-multi-2d fixnum f32 fixnum f32) f32)
(defun sample-hybrid-multi-2d (sampler xi xf yi yf)
  (declare (optimize (speed 3)))
  (let* ((func (fractal-2d-source-sample-func sampler))
         (offsets (fractal-2d-offsets sampler))
         (frequency (fractal-2d-frequency sampler))
         (lacunarity (fractal-2d-lacunarity sampler))
         (persistence (fractal-2d-persistence sampler))
         (scale (fractal-2d-scale sampler))
         (x-offset (aref offsets 0))
         (y-offset (aref offsets 1)))
    (loop :with result :of-type f32 := 0f0
          :with weight :of-type f32 := 1f0
          :for source :across (fractal-2d-sources sampler)
          :for fx :of-type f32 := (* (+ xi xf) frequency) :then (+ (* fx lacunarity) x-offset)
          :for fy :of-type f32 := (* (+ yi yf) frequency) :then (+ (* fy lacunarity) y-offset)
          :for amplitude :of-type f32 := 1f0 :then (* amplitude persistence)
          :while (> weight 0.001f0)
          :do (let* ((sample (multiple-value-call func source (floor fx) (floor fy)))
                     (signal (* (+ sample +hybrid-multi/offset+) amplitude)))
                (setf weight (min weight 1f0))
                (incf result (* weight signal))
                (setf weight (* weight signal)))
          :finally (return (1- (* (- result +hybrid-multi/minimum+) scale))))))

(defmethod sample-function ((sampler hybrid-multi-2d))
  #'sample-hybrid-multi-2d)
