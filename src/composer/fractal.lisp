(in-package #:mfiano.lib.gen.noise)

(deftype fractal-octaves () '(integer 1 32))

(define-constant +hybrid-multi/offset+ 0.7f0 :test #'=)

(define-constant +hybrid-multi/minimum+ (- +hybrid-multi/offset+ 1f0) :test #'=)

(define-ftype %floor-coordinate ((double-float (#.(- (expt 2d0 61))) (#.(expt 2d0 61))))
    (values fixnum f64))
(declaim (inline %floor-coordinate))
(defun %floor-coordinate (coordinate)
  (let* ((truncated (truncate coordinate))
         (floored (if (< coordinate (f64 truncated)) (1- truncated) truncated)))
    (values floored (- coordinate (f64 floored)))))

(define-ftype scale-coordinate (fixnum f64 f32 f32) (values fixnum f64))
(declaim (inline scale-coordinate))
(defun scale-coordinate (integer fraction factor shift)
  (let* ((factor (f64 factor))
         (whole (f64 integer))
         (product (* whole factor)))
    (if (< (abs product) #.(expt 2d0 26))
        (%floor-coordinate (+ product (* fraction factor) shift))
        (multiple-value-bind (high low) (two-product whole factor)
          (let ((floored (values (%floor-coordinate high))))
            (multiple-value-bind (carry rest)
                (%floor-coordinate (+ (- high (f64 floored)) low (* fraction factor) shift))
              (values (+ floored carry) rest)))))))

(define-ftype octave-coordinate (fixnum f64) (values fixnum f32))
(declaim (inline octave-coordinate))
(defun octave-coordinate (integer fraction)
  (let ((single (f32 fraction)))
    (if (< single 1f0)
        (values integer single)
        (values (1+ integer) 0f0))))

(defmacro sample-octave (func source factor &rest axes)
  `(progn
     (setf ,@(loop :for (integer fraction shift) :in axes
                   :append `((values ,integer ,fraction)
                             (scale-coordinate ,integer ,fraction ,factor ,shift))))
     (multiple-value-call ,func
       ,source
       ,@(loop :for (integer fraction) :in axes
               :collect `(octave-coordinate ,integer ,fraction)))))

(defstruct (fractal-2d
            (:include sampler-2d)
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (source-func (constantly nil) :type function)
  (source-sample-func (constantly 0f0) :type (function (sampler-2d fixnum f32 fixnum f32) f32))
  (sources (make-array 0) :type simple-vector)
  (offsets (make-f32-array 2) :type (f32-array 2))
  (octaves 4 :type fractal-octaves)
  (frequency 1f0 :type f32)
  (lacunarity 2f0 :type f32)
  (persistence 0.5f0 :type f32)
  (scale 1f0 :type f32))

(defstruct (fractal-3d
            (:include sampler-3d)
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (source-func (constantly nil) :type function)
  (source-sample-func (constantly 0f0)
   :type (function (sampler-3d fixnum f32 fixnum f32 fixnum f32) f32))
  (sources (make-array 0) :type simple-vector)
  (offsets (make-f32-array 3) :type (f32-array 3))
  (octaves 4 :type fractal-octaves)
  (frequency 1f0 :type f32)
  (lacunarity 2f0 :type f32)
  (persistence 0.5f0 :type f32)
  (scale 1f0 :type f32))

(defgeneric make-fractal-scalar (sampler))

(defgeneric fractal-octaves (sampler)
  (:method ((sampler fractal-2d))
    (fractal-2d-octaves sampler))
  (:method ((sampler fractal-3d))
    (fractal-3d-octaves sampler)))

(defgeneric fractal-persistence (sampler)
  (:method ((sampler fractal-2d))
    (fractal-2d-persistence sampler))
  (:method ((sampler fractal-3d))
    (fractal-3d-persistence sampler)))

(defgeneric fractal-attenuation (sampler))

(defgeneric (setf fractal-offsets) (value sampler)
  (:method (value (sampler fractal-2d))
    (setf (fractal-2d-offsets sampler) value))
  (:method (value (sampler fractal-3d))
    (setf (fractal-3d-offsets sampler) value)))

(defgeneric (setf fractal-scale) (value sampler)
  (:method (value (sampler fractal-2d))
    (setf (fractal-2d-scale sampler) value))
  (:method (value (sampler fractal-3d))
    (setf (fractal-3d-scale sampler) value)))

(defgeneric fractal-source-func (sampler)
  (:method ((sampler fractal-2d))
    (fractal-2d-source-func sampler))
  (:method ((sampler fractal-3d))
    (fractal-3d-source-func sampler)))

(defgeneric fractal-sources (sampler)
  (:method ((sampler fractal-2d))
    (fractal-2d-sources sampler))
  (:method ((sampler fractal-3d))
    (fractal-3d-sources sampler)))

(defgeneric (setf fractal-sources) (value sampler)
  (:method (value (sampler fractal-2d))
    (setf (fractal-2d-sources sampler) value))
  (:method (value (sampler fractal-3d))
    (setf (fractal-3d-sources sampler) value)))

(defgeneric (setf fractal-source-sample-func) (value sampler)
  (:method (value (sampler fractal-2d))
    (setf (fractal-2d-source-sample-func sampler) value))
  (:method (value (sampler fractal-3d))
    (setf (fractal-3d-source-sample-func sampler) value)))

(defun prepare-fractal-sampler (sampler)
  (let* ((rng (rng sampler))
         (octaves (fractal-octaves sampler))
         (source-func (fractal-source-func sampler))
         (source-sampler (funcall source-func))
         (dimensions (dimensions sampler)))
    (setf (fractal-sources sampler) (make-array octaves)
          (fractal-source-sample-func sampler) (sample-function source-sampler)
          (fractal-offsets sampler) (rng:vector/f32 rng dimensions)
          (fractal-scale sampler) (make-fractal-scalar sampler))
    (check-type source-sampler sampler)
    (assert-sampler-dimensions source-sampler dimensions)
    (dotimes (i octaves)
      (let ((source-sampler (funcall source-func)))
        (setf (seed source-sampler) (rng:u64 rng)
              (aref (fractal-sources sampler) i) source-sampler)))))

(defmethod (setf seed) :after (value (sampler fractal-2d))
  (prepare-fractal-sampler sampler))

(defmethod (setf seed) :after (value (sampler fractal-3d))
  (prepare-fractal-sampler sampler))

(defun %make-fractal-scalar (sampler)
  (loop :for i :below (fractal-octaves sampler)
        :sum (expt (fractal-persistence sampler) i) :into result
        :finally (return (/ result))))

(defun %make-fractal-scalar/multi (sampler)
  (loop :for i :below (fractal-octaves sampler)
        :for result := 1f0 :then (+ result (* result (expt (fractal-persistence sampler) i)))
        :finally (return (/ result))))

(defun %make-fractal-scalar/hybrid-multi (sampler)
  (loop :with result := 0f0
        :with weight := 1f0
        :for octave :below (fractal-octaves sampler)
        :for amplitude := 1f0 :then (* amplitude (fractal-persistence sampler))
        :while (> weight 0.001f0)
        :do (let ((signal (* (+ 1f0 +hybrid-multi/offset+) amplitude)))
              (setf weight (min weight 1f0))
              (incf result (* weight signal))
              (setf weight (* weight signal)))
        :finally (return (/ 2f0 (- result +hybrid-multi/minimum+)))))

(defun %make-fractal-scalar/ridged (sampler)
  (loop :repeat (fractal-octaves sampler)
        :for amplitude := 1f0 :then (* amplitude (fractal-persistence sampler))
        :for weight := 1f0 :then (s32:clamp (/ sample (fractal-attenuation sampler)) 0f0 1f0)
        :for sample := (* weight amplitude)
        :sum sample :into result
        :finally (return (/ 2f0 result))))
