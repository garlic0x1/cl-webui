(asdf:defsystem "cl-webui"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi)
  :components ((:file "webui")))

(asdf:defsystem "cl-webui/examples"
  :depends-on (:cffi :cl-webui :str :hiccl)
  :components ((:file "examples/minimal")
               (:file "examples/call-js-from-lisp")
               (:file "examples/call-lisp-from-js")))
