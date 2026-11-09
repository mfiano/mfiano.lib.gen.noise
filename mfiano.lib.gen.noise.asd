(asdf:defsystem #:mfiano.lib.gen.noise
  :description "A library for generating 2D and 3D coherent noise"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mfiano.lib.gen.noise"
  :encoding :utf-8
  :depends-on
  (#:mfiano.lib.base
   #:mfiano.lib.gen.rng
   #:mfiano.lib.gfx.pixel-buffer
   #:mfiano.lib.math.scalar)
  :in-order-to ((test-op (test-op #:mfiano.lib.gen.noise/test)))
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "sampler")
   (:file "field")
   (:file "noise/perlin")
   (:file "noise/perlin-2d")
   (:file "noise/perlin-3d")
   (:file "noise/simplex-2d")
   (:file "noise/simplex-3d")
   (:file "noise/opensimplex2")
   (:file "noise/opensimplex2-2d")
   (:file "noise/opensimplex2-3d")
   (:file "noise/opensimplex2s-2d")
   (:file "noise/opensimplex2s-3d")
   (:file "noise/value")
   (:file "noise/value-2d")
   (:file "noise/value-3d")
   (:file "noise/value-cubic-2d")
   (:file "noise/value-cubic-3d")
   (:file "noise/worley")
   (:file "noise/worley-2d")
   (:file "noise/worley-3d")
   (:file "composer/fractal")
   (:file "composer/fbm-2d")
   (:file "composer/fbm-3d")
   (:file "composer/billow-2d")
   (:file "composer/billow-3d")
   (:file "composer/multi-2d")
   (:file "composer/multi-3d")
   (:file "composer/hybrid-multi-2d")
   (:file "composer/hybrid-multi-3d")
   (:file "composer/ridged-2d")
   (:file "composer/ridged-3d")))

(asdf:defsystem #:mfiano.lib.gen.noise/test
  :description "Unit tests for mfiano.lib.gen.noise"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mfiano.lib.gen.noise"
  :encoding :utf-8
  :depends-on
  (#:1am
   #:mfiano.lib.gen.noise
   #:mfiano.lib.gfx.pixel-buffer)
  :perform (test-op (o c) (symbol-call '#:1am '#:run))
  :pathname "test"
  :serial t
  :components
  ((:file "helpers")
   (:file "test")))
