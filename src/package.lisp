(in-package #:mfiano.lib.base.pkg)

(define-package #:mfiano.lib.gen.noise
  (:local-nicknames
   (#:rng #:mfiano.lib.gen.rng.xoshiro256++)
   (#:pb #:mfiano.lib.gfx.pixel-buffer)
   (#:s32 #:mfiano.lib.math.scalar.f32)
   (#:s64 #:mfiano.lib.math.scalar.f64))
  (:use #:mfiano.lib.base.util)
  ;; sampler types
  (:export
   #:billow-2d
   #:billow-3d
   #:fbm-2d
   #:fbm-3d
   #:hybrid-multi-2d
   #:hybrid-multi-3d
   #:multi-2d
   #:multi-3d
   #:opensimplex2-2d
   #:opensimplex2-3d
   #:opensimplex2s-2d
   #:opensimplex2s-3d
   #:perlin-2d
   #:perlin-3d
   #:ridged-2d
   #:ridged-3d
   #:simplex-2d
   #:simplex-3d
   #:value-2d
   #:value-3d
   #:value-cubic-2d
   #:value-cubic-3d
   #:worley-2d
   #:worley-3d)
  ;; sampler constructors
  (:export
   #:make-billow-2d
   #:make-billow-3d
   #:make-fbm-2d
   #:make-fbm-3d
   #:make-hybrid-multi-2d
   #:make-hybrid-multi-3d
   #:make-multi-2d
   #:make-multi-3d
   #:make-opensimplex2-2d
   #:make-opensimplex2-3d
   #:make-opensimplex2s-2d
   #:make-opensimplex2s-3d
   #:make-perlin-2d
   #:make-perlin-3d
   #:make-ridged-2d
   #:make-ridged-3d
   #:make-simplex-2d
   #:make-simplex-3d
   #:make-value-2d
   #:make-value-3d
   #:make-value-cubic-2d
   #:make-value-cubic-3d
   #:make-worley-2d
   #:make-worley-3d)
  ;; sampling functions
  (:export
   #:sample-billow-2d
   #:sample-billow-3d
   #:sample-fbm-2d
   #:sample-fbm-3d
   #:sample-hybrid-multi-2d
   #:sample-hybrid-multi-3d
   #:sample-multi-2d
   #:sample-multi-3d
   #:sample-opensimplex2-2d
   #:sample-opensimplex2-3d
   #:sample-opensimplex2s-2d
   #:sample-opensimplex2s-3d
   #:sample-perlin-2d
   #:sample-perlin-3d
   #:sample-ridged-2d
   #:sample-ridged-3d
   #:sample-simplex-2d
   #:sample-simplex-3d
   #:sample-value-2d
   #:sample-value-3d
   #:sample-value-cubic-2d
   #:sample-value-cubic-3d
   #:sample-worley-2d
   #:sample-worley-3d)
  ;; sampler accessors
  (:export
   #:dimensions
   #:sample-function
   #:seed)
  ;; noise field functions
  (:export
   #:make-field))
