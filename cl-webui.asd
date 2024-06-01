(asdf:defsystem "cl-webui"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi)
  :components ((:file "webui"))
  :build-operation "program-op"
  :build-pathname "test-webui"
  :entry-point "webui::test-webui")
