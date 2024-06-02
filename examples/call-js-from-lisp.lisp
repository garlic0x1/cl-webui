(defpackage :webui/examples/call-js-from-lisp
  (:use :cl :webui)
  (:export :run))
(in-package :webui/examples/call-js-from-lisp)

(defparameter *script* "
let count = 0;
function GetCount() {
  return count;
}
function SetCount(number) {
  document.getElementById('count').innerHTML = number;
  count = number;
}
function AutoTest(number) {
  setInterval(function(){ webui.call('MyButton1'); }, 10);
}")

(defparameter *html*
  (hiccl:render nil
    `(:html
      (:head
       (:script :src "webui.js")
       (:title "Call JavaScript from Lisp example"))
      (:body
       (:h1 "WebUI - Call JS from Lisp") (:br)
       (:h1 :id "count" "0") (:br)
       (:button :id "MyButton1" "Manual Count") (:br)
       (:button :id "MyTest" :onclick "AutoTest();" "Auto Count") (:br)
       (:button :id "MyButton2" "Exit")
       (:script (:raw ,*script*))))))

(defun run ()
  (let ((w (webui-new-window)))
    (webui-bind w "MyButton1"
                (lambda (ev)
                  (let* ((w (webui-event-window ev))
                         (c (webui-script w "return GetCount();" 1000))
                         (n (1+ (parse-integer c))))
                    (webui-run w (format nil "SetCount(~a);" n)))))
    (webui-bind w "MyButton2"
                (lambda (ev)
                  (declare (ignore ev))
                  (webui-close w)))
    (webui-show w *html*)
    (webui-wait)
    (webui-destroy w)))
