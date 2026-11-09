(in-package #:mfiano.lib.gen.noise/test)

(test field.make-field.rejects-non-sampler
  (signals error (n:make-field 42 :width 4 :height 4)))

(test field.make-field.rejects-empty-ranges
  (let ((sampler (n:make-perlin-2d)))
    (signals error (n:make-field sampler :width 4 :height 4 :x-min 1 :x-max 1))
    (signals error (n:make-field sampler :width 4 :height 4 :x-min 1 :x-max -1))
    (signals error (n:make-field sampler :width 4 :height 4 :y-min 1 :y-max 1))
    (signals error (n:make-field sampler :width 4 :height 4 :y-min 1 :y-max -1))))

(test field.fill.2d
  (let* ((sampler (n:make-perlin-2d :seed 1))
         (sample #'n:sample-perlin-2d)
         (field (n:make-field sampler :width 8 :height 4 :x-min -3/2 :x-max 5/2 :y-min 2 :y-max 4)))
    (flet ((expected (x y)
             (gray (sample-at sample sampler (+ -3/2 (* x 1/2)) (+ 2 (* y 1/2))))))
      (dotimes (y 4)
        (is (equal (fill-row field 0 y 8)
                   (loop :for x :below 8 :collect (expected x y))))))))

(test field.fill.3d
  (let* ((sampler (n:make-simplex-3d :seed 1))
         (sample #'n:sample-simplex-3d)
         (field (n:make-field sampler :width 8 :height 4)))
    (flet ((expected (x y)
             (gray (sample-at sample sampler (+ -1 (* x 1/4)) (+ -1 (* y 1/2)) 0 0.5f0))))
      (dotimes (y 4)
        (is (equal (fill-row field 0 y 8)
                   (loop :for x :below 8 :collect (expected x y))))))))

(test field.fill.partial-row
  (let* ((sampler (n:make-perlin-2d :seed 1))
         (sample #'n:sample-perlin-2d)
         (field (n:make-field sampler :width 8 :height 4)))
    (is (equal (fill-row field 3 2 4)
               (loop :for x :from 3 :below 7
                     :collect (gray (sample-at sample sampler (+ -1 (* x 1/4)) 0)))))))

(test field.fill.large-coordinates
  (let* ((origin (expt 2 24))
         (sampler (n:make-perlin-2d :seed 1))
         (sample #'n:sample-perlin-2d)
         (field (n:make-field sampler :width 8 :height 1 :x-min origin :x-max (+ origin 1))))
    (is (equal (fill-row field 0 0 8)
               (loop :for x :below 8
                     :collect (gray (sample-at sample sampler (+ origin (* x 1/8)) -1)))))))

(test noise.value-cubic-2d.seeds-change-output
  (flet ((sample (seed)
           (sample-at #'n:sample-value-cubic-2d (n:make-value-cubic-2d :seed seed) 3.25 5.5)))
    (is (/= (sample 1) (sample 42)))))

(test noise.value-cubic-2d.seeds-change-octaves
  (flet ((sample (seed)
           (sample-at #'n:sample-fbm-2d
                      (n:make-fbm-2d #'n:make-value-cubic-2d :seed seed :octaves 1)
                      3.25
                      5.5)))
    (is (/= (sample 1) (sample 42)))))

(test noise.value-cubic-3d.seeds-change-output
  (flet ((sample (seed)
           (sample-at #'n:sample-value-cubic-3d
                      (n:make-value-cubic-3d :seed seed)
                      3.25
                      5.5
                      0
                      0.5f0)))
    (is (/= (sample 1) (sample 42)))))

(test noise.value-cubic-3d.is-a-sampler
  (let ((fbm (n:make-fbm-3d #'n:make-value-cubic-3d :seed 1 :octaves 2))
        (field (n:make-field (n:make-value-cubic-3d :seed 1) :width 2 :height 2)))
    (is (functionp field))
    (is (< -1 (sample-at #'n:sample-fbm-3d fbm 3.25 5.5 0 0.5f0) 1))))

(test noise.worley-2d.euclidean-is-the-distance
  (flet ((sample (metric)
           (1+ (sample-at #'n:sample-worley-2d
                          (n:make-worley-2d :seed 1 :metric metric)
                          3.25
                          5.5))))
    (is (< (abs (- (sample :euclidean) (sqrt (sample :euclidean-squared)))) 1f-6))))

(test noise.worley-3d.euclidean-is-the-distance
  (flet ((sample (metric)
           (1+ (sample-at #'n:sample-worley-3d
                          (n:make-worley-3d :seed 1 :metric metric)
                          3.25
                          5.5
                          0
                          0.5f0))))
    (is (< (abs (- (sample :euclidean) (sqrt (sample :euclidean-squared)))) 1f-6))))

(test noise.perlin.tables-are-permutations
  (dotimes (seed 10)
    (let ((table (n::perlin-2d-table (n:make-perlin-2d :seed seed))))
      (is (equal (sort (coerce (subseq table 0 256) 'list) #'<)
                 (loop :for i :below 256 :collect i)))
      (is (equalp (subseq table 0 256) (subseq table 256))))))

(test noise.perlin.tables-are-not-only-single-cycles
  (is (loop :for seed :below 10
            :for table := (n::perlin-2d-table (n:make-perlin-2d :seed seed))
            :thereis (loop :for i :below 256
                           :thereis (= (aref table i) i)))))

(test noise.simplex-2d.gradients-are-even
  (let ((sampler (n:make-simplex-2d :seed 1))
        (octants nil)
        (magnitudes nil))
    (dotimes (i 8)
      (dotimes (j 8)
        (multiple-value-bind (angle magnitude) (simplex-2d-gradient sampler i j)
          (multiple-value-bind (octant offset) (round (- angle 22.5d0) 45)
            (is (< (abs offset) 0.5d0))
            (pushnew octant octants)
            (push magnitude magnitudes)))))
    (is (= (length octants) 8))
    (is (< (/ (reduce #'max magnitudes) (reduce #'min magnitudes)) 1.01d0))))

(test composer.hybrid-multi-2d.matches-musgrave
  (let ((sampler (n:make-hybrid-multi-2d #'n:make-value-2d :seed 1 :octaves 6 :persistence 0.8)))
    (dolist (point '((3.25 5.5) (-7.75 0.125) (12.5 -3.375) (0.0625 0.9375)))
      (destructuring-bind (x y) point
        (is (< (abs (- (sample-at #'n:sample-hybrid-multi-2d sampler x y)
                       (hybrid-multi-reference sampler #'n:sample-value-2d x y)))
               1f-5))))))

(test composer.hybrid-multi-3d.matches-musgrave
  (let ((sampler (n:make-hybrid-multi-3d #'n:make-value-3d :seed 1 :octaves 6 :persistence 0.8)))
    (dolist (point '((3.25 5.5 0.5) (-7.75 0.125 2.25) (12.5 -3.375 -1.5) (0.0625 0.9375 7)))
      (destructuring-bind (x y z) point
        (is (< (abs (- (apply #'n:sample-hybrid-multi-3d sampler (split-coordinates point))
                       (hybrid-multi-reference sampler #'n:sample-value-3d x y z)))
               1f-5))))))

(test composer.hybrid-multi-2d.one-octave-is-the-source
  (let* ((sampler (n:make-hybrid-multi-2d #'n:make-value-2d :seed 1 :octaves 1))
         (source (aref (n::fractal-sources sampler) 0)))
    (dolist (point '((3.25 5.5) (-7.75 0.125)))
      (is (< (abs (- (apply #'sample-at #'n:sample-hybrid-multi-2d sampler point)
                     (apply #'sample-at #'n:sample-value-2d source point)))
             1f-6)))))

(test composer.hybrid-multi-2d.smooth-where-low
  (let* ((sampler (n:make-hybrid-multi-2d #'n:make-value-2d :seed 1 :octaves 8 :persistence 0.8))
         (source (aref (n::fractal-sources sampler) 0))
         (scale (n::fractal-2d-scale sampler))
         (low-points 0))
    (dotimes (i 60)
      (dotimes (j 60)
        (let* ((x (* i 0.37d0))
               (y (* j 0.41d0))
               (base (sample-at #'n:sample-value-2d source x y)))
          (when (< base -0.71)
            (incf low-points)
            (is (< (abs (- (sample-at #'n:sample-hybrid-multi-2d sampler x y)
                           (1- (* (+ base 1) scale))))
                   1f-5))))))
    (is (> low-points 10))))
