(defpackage :webui/examples/call-lisp-from-js
  (:use :cl :webui)
  (:export :run))
(in-package :webui/examples/call-lisp-from-js)

(defparameter *html*
  (hiccl:render nil
    `(:html
      (:head (:script :src "webui.js"))
      (:body
       (:h1 "WebUI - Call Lisp from JavaScript")
       (:p "C functions with arguments" (:em "See the logs in your terminal"))
       (:button :onclick "webui.call('MyID_One', 'hello', 'world');"
        "Strings")
       (:br)
       (:button :onclick "webui.call('MyID_Two', 123, 456, 789, 12345.6789);"
        "Numbers")
       (:br))
      (:script "const arr_size = 512 * 1000;
                const big_arr = new Uint8Array(arr_size);
                big_arr[0] = 0xA1;
                big_arr[arr_size - 1] = 0xA2;
                function MyJS() {
                  const MyInput = document.getElementById('MyInputID');
                  const number = MyInput.value;
                  webui.call('MyID_Four', number, 2).then((response) => {
                      MyInput.value = response;
                  });
                }"))))

(defun alert (w obj)
  (webui-run w "console.log('alerting');")
  (webui-run w (format nil "console.log(btoa('~a'));"
                       (webui-encode (format nil "~a" obj))))
  (webui-run w (format nil "alert(btoa('~a'));"
                       (webui-encode (format nil "~a" obj)))))

(defun run ()
  (let ((w (webui-new-window)))
    (webui-bind w "MyID_One"
                (lambda (ev)
                  (alert w (format nil "One: ~a, ~a~%"
                                   (webui-get-string ev) ;; first
                                   (webui-get-string-at ev 1)))))
    (webui-bind w "MyID_Two"
                (lambda (ev)
                  (alert w (format nil
                                   "Two: c: ~a, a: ~a, b: ~a, c: ~a, f: ~a~%"
                                   nil ;; (webui-get-count ev)
                                   (webui-get-int ev) ;; first
                                   (webui-get-int-at ev 1)
                                   (webui-get-int-at ev 2)
                                   (webui-get-float-at ev 3)))))
    (webui-show w *html*)
    (webui-wait)
    (webui-destroy w)))
