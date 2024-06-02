# Overview

Bindings for [webui](https://github.com/webui-dev/webui)

```lisp
(let ((w (webui-new-window)))
  (webui-bind w "my-button"
              (lambda (event)
                (declare (ignore event))
                (webui-run w "alert(\"hi\");")))
  (webui-show w "<html>
                   <script src=\"webui.js\"></script>
                   Hello,
                   <button id=\"my-button\">world!</button>
                 </html>")
  (webui-wait))
```

![demo-1](screenshots/demo-1.png)
