(in-package #:mfiano.lib.gen.noise)

(defstruct (sampler
            (:constructor nil)
            (:conc-name "")
            (:predicate sampler?)
            (:copier nil))
  (dimensions 2 :type (integer 2 3) :read-only t)
  (%seed 0 :type u64)
  (rng (rng:make-rng 0) :type rng:rng))

(defstruct (sampler-2d
            (:include sampler
             (dimensions 2))
            (:constructor nil)
            (:predicate sampler-2d?)
            (:copier nil)))

(defstruct (sampler-3d
            (:include sampler
             (dimensions 3))
            (:constructor nil)
            (:predicate sampler-3d?)
            (:copier nil)))

(define-printer (sampler stream :type t :identity t)
  (format stream "(seed: ~d)" (%seed sampler)))

(defgeneric seed (sampler)
  (:method ((sampler sampler))
    (%seed sampler)))

(defgeneric (setf seed) (value sampler)
  (:method ((value integer) (sampler sampler))
    (check-type value u64)
    (let ((rng (rng:make-rng value)))
      (setf (%seed sampler) value
            (rng sampler) rng)
      value)))

(defgeneric sample-function (sampler)
  (:method (sampler)))

(defun assert-sampler (sampler)
  (unless (sampler? sampler)
    (error "Invalid sampler: ~s" sampler)))

(defun assert-sampler-dimensions (sampler required-dimensions)
  (assert-sampler sampler)
  (let ((dimensions (dimensions sampler)))
    (unless (= required-dimensions dimensions)
      (error "~dD sampler required. Got ~dD (~a)." required-dimensions dimensions sampler))))
