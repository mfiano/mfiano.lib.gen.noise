(in-package #:mfiano.lib.gen.noise)

(defstruct (hybrid-multi-3d
            (:include fractal-3d)
            (:constructor %make-hybrid-multi-3d)
            (:predicate nil)
            (:copier nil)))

(defmethod make-fractal-scalar ((sampler hybrid-multi-3d))
  (%make-fractal-scalar/hybrid-multi sampler))

(defun make-hybrid-multi-3d (source
                             &key (seed 0) (octaves 4) (frequency 1) (lacunarity 2)
                               (persistence 0.84))
  (let ((sampler (%make-hybrid-multi-3d :source-func source
                                        :octaves octaves
                                        :frequency (f32 frequency)
                                        :lacunarity (f32 lacunarity)
                                        :persistence (f32 persistence))))
    (setf (seed sampler) seed)
    sampler))

(define-ftype sample-hybrid-multi-3d (hybrid-multi-3d fixnum f32 fixnum f32 fixnum f32) f32)
(defun sample-hybrid-multi-3d (sampler xi xf yi yf zi zf)
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
    (loop :with result :of-type f32 := 0f0
          :with weight :of-type f32 := 1f0
          :for source :across (fractal-3d-sources sampler)
          :for factor :of-type f32 := frequency :then lacunarity
          :for x-shift :of-type f32 := 0f0 :then x-offset
          :for y-shift :of-type f32 := 0f0 :then y-offset
          :for z-shift :of-type f32 := 0f0 :then z-offset
          :for amplitude :of-type f32 := 1f0 :then (* amplitude persistence)
          :while (> weight 0.001f0)
          :do (let* ((sample (sample-octave func
                                            source
                                            factor
                                            (x-integer x-fraction x-shift)
                                            (y-integer y-fraction y-shift)
                                            (z-integer z-fraction z-shift)))
                     (signal (* (+ sample +hybrid-multi/offset+) amplitude)))
                (setf weight (min weight 1f0))
                (incf result (* weight signal))
                (setf weight (* weight signal)))
          :finally (return (1- (* (- result +hybrid-multi/minimum+) scale))))))

(defmethod sample-function ((sampler hybrid-multi-3d))
  #'sample-hybrid-multi-3d)
