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
         (octaves (fractal-2d-octaves sampler))
         (frequency (fractal-2d-frequency sampler))
         (lacunarity (fractal-2d-lacunarity sampler))
         (persistence (fractal-2d-persistence sampler))
         (scale (fractal-2d-scale sampler))
         (x-offset (aref offsets 0))
         (y-offset (aref offsets 1))
         (sources (fractal-2d-sources sampler)))
    (loop :with fx := (* (+ xi xf) frequency)
          :with fy := (* (+ yi yf) frequency)
          :with source1 := (aref sources 0)
          :with sample1 := (multiple-value-call func source1 (floor fx) (floor fy))
          :with result := (* sample1 persistence)
          :for i :from 1 :below octaves
          :for source := (aref sources i)
          :for amplitude := persistence :then (* amplitude persistence)
          :for weight :of-type f32 := result :then (max weight 1f0)
          :for lx := (+ (* fx lacunarity) x-offset)
          :for ly := (+ (* fy lacunarity) y-offset)
          :for sample :of-type f32 := (multiple-value-call func source (floor lx) (floor ly))
          :do (setf weight (* weight sample amplitude))
              (incf result weight)
          :finally (return (* result scale)))))

(defmethod sample-function ((sampler hybrid-multi-2d))
  #'sample-hybrid-multi-2d)
