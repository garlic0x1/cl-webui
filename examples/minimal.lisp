(defpackage :webui/examples/minimal
  (:use :cl :webui)
  (:export :run))
(in-package :webui/examples/minimal)

(defun run ()
  (let ((w (webui-new-window)))
    (webui-show w "<html>Hello, world!</html>")
    (webui-wait)
    (webui-clean)))
