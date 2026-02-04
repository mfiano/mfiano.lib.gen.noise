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
                               (persistence 0.25))
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
         (octaves (fractal-3d-octaves sampler))
         (frequency (fractal-3d-frequency sampler))
         (lacunarity (fractal-3d-lacunarity sampler))
         (persistence (fractal-3d-persistence sampler))
         (scale (fractal-3d-scale sampler))
         (x-offset (aref offsets 0))
         (y-offset (aref offsets 1))
         (z-offset (aref offsets 2))
         (sources (fractal-3d-sources sampler)))
    (loop :with fx := (* (+ xi xf) frequency)
          :with fy := (* (+ yi yf) frequency)
          :with fz := (* (+ zi zf) frequency)
          :with source1 := (aref sources 0)
          :with sample1 := (multiple-value-call func source1 (floor fx) (floor fy) (floor fz))
          :with result := (* sample1 persistence)
          :for i :from 1 :below octaves
          :for source := (aref sources i)
          :for amplitude := persistence :then (* amplitude persistence)
          :for weight :of-type f32 := result :then (max weight 1f0)
          :for lx := (+ (* fx lacunarity) x-offset)
          :for ly := (+ (* fy lacunarity) y-offset)
          :for lz := (+ (* fz lacunarity) z-offset)
          :for sample :of-type f32 := (multiple-value-call func
                                        source
                                        (floor lx)
                                        (floor ly)
                                        (floor lz))
          :do (setf weight (* weight sample amplitude))
              (incf result weight)
          :finally (return (* result scale)))))

(defmethod sample-function ((sampler hybrid-multi-3d))
  #'sample-hybrid-multi-3d)
