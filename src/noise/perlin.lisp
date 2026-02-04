(in-package #:mfiano.lib.gen.noise)

(defun make-lookup-table/perlin (sampler)
  (let ((table (make-u8-array 512))
        (rng (rng sampler)))
    (dotimes (i 256)
      (setf (aref table i) i))
    (loop :for i :from 255 :downto 1
          :for j := (rng:range rng 0 i)
          :do (rotatef (aref table i) (aref table j)))
    (replace table table :start1 256)
    table))
