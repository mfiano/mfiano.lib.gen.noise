(in-package #:mfiano.lib.gen.noise)

(defstruct (opensimplex2s-2d
            (:include sampler-2d)
            (:constructor %make-opensimplex2s-2d)
            (:predicate nil)
            (:copier nil))
  (table (make-f64-array 512) :type (f64-array 512))
  (orientation nil :type opensimplex2-2d/orientation))

(defmethod (setf seed) :after (value (sampler opensimplex2s-2d))
  (setf (opensimplex2s-2d-table sampler) (make-lookup-table/opensimplex2 sampler)))

(defun make-opensimplex2s-2d (&key (seed 0) orient)
  (check-type seed u64)
  (check-type orient opensimplex2-2d/orientation)
  (let ((sampler (%make-opensimplex2s-2d :orientation orient)))
    (setf (seed sampler) seed)
    sampler))

(defmethod opensimplex2/gradient-scale ((sampler opensimplex2s-2d))
  0.054818663746118546d0)

(define-ftype sample-opensimplex2s-2d (opensimplex2s-2d fixnum f32 fixnum f32) f32)
(declaim (inline sample-opensimplex2s-2d))
(defun sample-opensimplex2s-2d (sampler xi xf yi yf)
  (declare (optimize (speed 3)))
  (flet ((c (table seed x1 y1 x2 y2)
           (let ((a (- #.(/ 2d0 3d0) (* x2 x2) (* y2 y2))))
             (if (plusp a)
                 (* a a a a (opensimplex2-2d/grad table seed x1 y1 x2 y2))
                 0d0))))
    (declare (inline c))
    (let ((orientation (opensimplex2s-2d-orientation sampler))
          (table (opensimplex2s-2d-table sampler))
          (seed (%seed sampler))
          (x (+ xi (f64 xf)))
          (y (+ yi (f64 yf))))
      (multiple-value-bind (trx try) (opensimplex2-2d/orient orientation x y)
        (let* ((x1 (floor trx))
               (y1 (floor try))
               (xs (- trx x1))
               (ys (- try y1))
               (d (- xs ys))
               (u (* (+ xs ys) +opensimplex2-2d/unskew+))
               (x2 (u64* x1 +prime-x+))
               (y2 (u64* y1 +prime-y+))
               (x3 (u64+ x2 +prime-x+))
               (y3 (u64+ y2 +prime-y+))
               (x4 (+ xs u))
               (y4 (+ ys u))
               (x5 (- x4 #.(1+ (* +opensimplex2-2d/unskew+ 2))))
               (y5 (- y4 #.(1+ (* +opensimplex2-2d/unskew+ 2))))
               (a1 (- #.(/ 2d0 3d0) (* x4 x4) (* y4 y4)))
               (a2 (+ (* #.(* (1+ (* +opensimplex2-2d/unskew+ 2))
                              (+ (/ +opensimplex2-2d/unskew+) 2)
                              2)
                         u)
                      #.(* (1+ (* +opensimplex2-2d/unskew+ 2))
                           (1+ (* +opensimplex2-2d/unskew+ 2))
                           -2)
                      a1))
               (c1 (* a1 a1 a1 a1 (opensimplex2-2d/grad table seed x2 y2 x4 y4)))
               (c2 (* a2 a2 a2 a2 (opensimplex2-2d/grad table seed x3 y3 x5 y5)))
               (r (+ c1 c2)))
          (declare (type i64 x1 y1))
          (cond
            ((< u +opensimplex2-2d/unskew+)
             (let ((x (u64+ x2 #.(shl +prime-x+ 1)))
                   (y (u64+ y2 #.(shl +prime-y+ 1))))
               (if (> (+ xs d) 1)
                   (incf r (c table
                              seed
                              x
                              y3
                              (- x4 #.(+ (* +opensimplex2-2d/unskew+ 3) 2))
                              (- y4 #.(1+ (* +opensimplex2-2d/unskew+ 3)))))
                   (incf r (c table
                              seed
                              x2
                              y3
                              (- x4 +opensimplex2-2d/unskew+)
                              (- y4 #.(1+ +opensimplex2-2d/unskew+)))))
               (if (> (- ys d) 1)
                   (incf r (c table
                              seed
                              x3
                              y
                              (- x4 #.(1+ +opensimplex2-2d/unskew+))
                              (- y4 #.(+ (* +opensimplex2-2d/unskew+ 3) 2))))
                   (incf r (c table
                              seed
                              x3
                              y2
                              (- x4 #.(1+ +opensimplex2-2d/unskew+))
                              (- y4 +opensimplex2-2d/unskew+))))))
            (t
             (let ((x (u64- x2 +prime-x+))
                   (y (u64- y2 +prime-y+)))
               (if (minusp (+ xs d))
                   (incf r (c table
                              seed
                              x
                              y2
                              (+ x4 #.(1+ +opensimplex2-2d/unskew+))
                              (+ y4 +opensimplex2-2d/unskew+)))
                   (incf r (c table
                              seed
                              x3
                              y2
                              (- x4 #.(1+ +opensimplex2-2d/unskew+))
                              (- y4 +opensimplex2-2d/unskew+))))
               (if (< ys d)
                   (incf r (c table
                              seed
                              x2
                              y
                              (+ x4 +opensimplex2-2d/unskew+)
                              (+ y4 #.(1+ +opensimplex2-2d/unskew+))))
                   (incf r (c table
                              seed
                              x2
                              y3
                              (- x4 +opensimplex2-2d/unskew+)
                              (- y4 #.(1+ +opensimplex2-2d/unskew+))))))))
          (f32 r))))))

(defmethod sample-function ((sampler opensimplex2s-2d))
  #'sample-opensimplex2s-2d)
