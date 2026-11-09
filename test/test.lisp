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
