(in-package #:mfiano.lib.gen.noise)

(declaim (inline value-hash-2d))
(defun value-hash-2d (seed x y)
  (let ((hash (u64* +hash1+ (expt (logxor seed x y) 2))))
    (/ (u32 (logxor hash (shr hash 19))) +hash2+ 1f0)))

(declaim (inline value-hash-3d))
(defun value-hash-3d (seed x y z)
  (let ((hash (u64* +hash1+ (expt (logxor seed x y z) 2))))
    (/ (u32 (logxor hash (shr hash 19))) +hash2+ 1f0)))
