(defpackage :webui
  (:use :cl :cffi)
  (:export :webui-event-t
           :webui-event-window
           :webui-event-event-type
           :webui-event-element-id
           :webui-event-event-number
           :webui-event-bind-id
           :webui-new-window
           :webui-new-window-id
           :webui-get-new-window-id
           :webui-bind
           :webui-show
           :webui-show-browser
           :webui-show-wb
           :webui-show-kiosk
           :webui-wait
           :webui-close
           :webui-destroy
           :webui-exit
           :webui-set-root-folder
           :webui-set-default-root-folder
           :webui-set-file-handler
           :webui-is-shown
           :webui-set-timeout
           :webui-set-icon
           :webui-encode
           :webui-decode
           :webui-free
           :webui-malloc
           :webui-send-raw
           :webui-set-hide
           :webui-set-size
           :webui-set-position
           :webui-set-profile
           :webui-set-proxy
           :webui-get-url
           :webui-set-public
           :webui-navigate
           :webui-clean
           :webui-delete-all-profiles
           :webui-delete-profile
           :webui-get-parent-process-id
           :webui-get-child-process-id
           :webui-set-port
           :webui-config
           :webui-set-tls-certificate
           :webui-run
           :webui-script
           :webui-set-runtime
           :webui-get-count
           :webui-get-int-at
           :webui-get-int
           :webui-get-float-at
           :webui-get-float
           :webui-get-string-at
           :webui-get-string
           :webui-get-bool-at
           :webui-get-bool
           :webui-get-size-at
           :webui-get-size
           :webui-return-int
           :webui-return-float
           :webui-return-string
           :webui-return-bool
           :webui-interface-bind
           :webui-interface-set-response
           :webui-interface-is-app-running
           :webui-interface-get-window-id
           :webui-interface-get-string-at
           :webui-interface-get-int-at
           :webui-interface-get-bool-at
           :webui-interface-get-size-at
           ))
(in-package :webui)

(use-foreign-library "webui-2.so")

(defctype size-t :unsigned-int)

(defcstruct webui-event-t
  (window size-t)
  (event-type :unsigned-int)
  (element-id :pointer)
  (event-number :unsigned-int)
  (bind-id :unsigned-int))

(defstruct webui-event
  window
  event-type
  element-id
  event-number
  bind-id)

(defun translate-webui-event (event)
  (with-foreign-slots ((window event-type element-id event-number bind-id)
                       event
                       (:struct webui-event-t))
    (make-webui-event :window window
                      :event-type event-type
                      :element-id element-id
                      :event-number event-number
                      :bind-id bind-id)))

(defcfun "webui_new_window" size-t
  "@brief Create a new WebUI window object.

@return Returns the window number.

@example size_t myWindow = webui_new_window();")

(defcfun "webui_new_window_id" size-t
  "@brief Create a new webui window object using a specified window number.

@param window_number The window number (should be > 0, and < WEBUI_MAX_IDS)

@return Returns the window number.

@example size_t myWindow = webui_new_window_id(123);"
  (window-number size-t))

(defcfun "webui_get_new_window_id" size-t
  "@brief Get a free window number that can be used with
`webui_new_window_id()`.

@return Returns the first available free window number. Starting from 1.

@example size_t myWindowNumber = webui_get_new_window_id();")

(defun webui-bind (window element-id func)
  "@brief Bind a specific html element click event with a function. Empty
element means all events.

@param window The window number
@param element The HTML ID
@param func The callback function

@return Returns a unique bind ID.

@example webui_bind(myWindow, \"myID\", myFunction);"
  (defcallback webui-bind-cb :void ((event :pointer))
    (funcall func (translate-webui-event event)))
  (foreign-funcall "webui_bind"
                   size-t window
                   :string element-id
                   :pointer (callback webui-bind-cb)
                   size-t))

(defcfun "webui_show" :bool
  "@brief Show a window using embedded HTML, or a file. If the window is already
open, it will be refreshed.

@param window The window number
@param content The HTML, URL, Or a local file

@return Returns True if showing the window is successed.

@example webui_show(myWindow, \"<html>...</html>\"); | webui_show(myWindow,
\"index.html\"); | webui_show(myWindow, \"http://...\");"
  (window size-t)
  (content :string))

(defcfun "webui_show_browser" :bool
  "@brief Same as `webui_show()`. But using a specific web browser.

@param window The window number
@param content The HTML, Or a local file
@param browser The web browser to be used

@return Returns True if showing the window is successed.

@example webui_show_browser(myWindow, \"<html>...</html>\", Chrome); |
webui_show(myWindow, \"index.html\", Firefox);"
  (window size-t)
  (content :string)
  (browser size-t))

(defcfun "webui_show_wv" :bool
  "@brief Show a WebView window using embedded HTML, or a file. If the window is already
open, it will be refreshed. Note: Win32 need `WebView2Loader.dll`.

@param window The window number
@param content The HTML, URL, Or a local file

@return Returns True if showing the WebView window is successed.

@example webui_show_wv(myWindow, \"<html>...</html>\"); | webui_show_wv(myWindow,
\"index.html\"); | webui_show_wv(myWindow, \"http://...\");"
  (window size-t)
  (content :string))

(defcfun "webui_set_kiosk" :void
  "@brief Set the window in Kiosk mode (Full screen)

@param window The window number
@param status True or False

@example webui_set_kiosk(myWindow, true);"
  (window size-t)
  (status :bool))

(defcfun "webui_wait" :void
  "@brief Wait until all opened windows get closed.

@example webui_wait(); ")

(defcfun "webui_close" :void
  "@brief Close a specific window only. The window object will still exist.

@param window The window number

@example webui_close(myWindow);"
  (window size-t))

(defcfun "webui_destroy" :void
  "@brief Close a specific window and free all memory resources.

@param window The window number

@example webui_destroy(myWindow);"
  (window size-t))

(defcfun "webui_exit" :void
  "@brief Close all open windows. `webui_wait()` will return (Break).

@example webui_exit();")

(defcfun "webui_set_root_folder" :bool
  "@brief Set the web-server root folder path for a specific window.

@param window The window number
@param path The local folder full path

@example webui_set_root_folder(myWindow, \"/home/Foo/Bar/\");"
  (window size-t)
  (path :string))

(defcfun "webui_set_default_root_folder" :bool
  "@brief Set the web-server root folder path for all windows. Should be used
before `webui_show()`.

@param path The local folder full path

@example webui_set_default_root_folder(\"/home/Foo/Bar/\");"
  (path :string))

(defcfun "webui_set_file_handler" :void
  "@param window The window number
@param handler The handler function: `void myHandler(const char* filename,
int* length)`

@return Returns a unique bind ID.

@example webui_set_file_handler(myWindow, myHandlerFunction);"
  (window size-t)
  (handler :pointer))

(defcfun "webui_is_shown" :bool
  "@brief Check if the specified window is still running.

@param window The window number

@example webui_is_shown(myWindow);"
  (window size-t))

(defcfun "webui_set_timeout" :void
  "@brief Set the maximum time in seconds to wait for the browser to start.

@param second The timeout in seconds

@example webui_set_timeout(30);"
  (second size-t))

(defcfun "webui_set_icon" :void
  "@brief Set the default embedded HTML favicon.

@param window The window number
@param icon The icon as string: `<svg>...</svg>`
@param icon_type The icon type: `image/svg+xml`

@example webui_set_icon(myWindow, \"<svg>...</svg>\", \"image/svg+xml\");"
  (window size-t)
  (icon :string)
  (icon-type :string))

(defcfun "webui_encode" :string
  "@brief Base64 encoding. Use this to safely send text based data to the UI. If
it fails it will return NULL.

@param str The string to encode (Should be null terminated)

@example webui_encode(\"Hello\");"
  (str :string))

(defcfun "webui_decode" :string
  "@brief Base64 decoding. Use this to safely decode received Base64 text from
the UI. If it fails it will return NULL.

@param str The string to decode (Should be null terminated)

@example webui_decode(\"SGVsbG8=\");"
  (str :string))

(defcfun "webui_free" :void
  "@brief Safely free a buffer allocated by WebUI using `webui_malloc()`.

@param ptr The buffer to be freed

@example webui_free(myBuffer);"
  (ptr :pointer))

(defcfun "webui_malloc" :pointer
  "@brief Safely allocate memory using the WebUI memory management system. It
can be safely freed using `webui_free()` at any time.

@param size The size of memory in bytes

@example char* myBuffer = (char*)webui_malloc(1024);"
  (size size-t))

(defcfun "webui_send_raw" :void
  "@brief Safely send raw data to the UI.

@param window The window number
@param function The JavaScript function to receive raw data: `function
myFunc(myData){}`
@param raw The raw data buffer
@param size The raw data size in bytes

@example webui_send_raw(myWindow, \"myJavascriptFunction\", myBuffer, 64);"
  (window size-t)
  (func :string)
  (raw :pointer)
  (size size-t))

(defcfun "webui_set_hide" :void
  "@brief Set a window in hidden mode. Should be called before `webui_show()`.

@param window The window number
@param status The status: True or False

@example webui_set_hide(myWindow, True);"
  (window size-t)
  (status :bool))

(defcfun "webui_set_size" :void
  "@brief Set the window size.

@param window The window number
@param width The window width
@param height The window height

@example webui_set_size(myWindow, 800, 600);"
  (window size-t)
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun "webui_set_position" :void
  "@brief Set the window position.

@param window The window number
@param x The window X
@param y The window Y

@example webui_set_position(myWindow, 100, 100);"
  (window size-t)
  (x :unsigned-int)
  (y :unsigned-int))

(defcfun "webui_set_profile" :void
  "@brief Set the web browser profile to use. An empty `name` and `path` means
the default user profile. Need to be called before `webui_show()`.

@param window The window number
@param name The web browser profile name
@param path The web browser profile full path

@example webui_set_profile(myWindow, \"Bar\", \"/Home/Foo/Bar\"); |
webui_set_profile(myWindow, \"\", \"\");"
  (window size-t)
  (name :string)
  (path :string))

(defcfun "webui_set_proxy" :void
  "@brief Set the web browser proxy_server to use. Need to be called before `webui_show()`.

@param window The window number
@param proxy_server The web browser proxy_server

@example webui_set_proxy(myWindow, \"http://127.0.0.1:8888\");"
  (window size-t)
  (proxy-server :string))

(defcfun "webui_get_url" :string
  "@brief Get the full current URL.

@param window The window number

@return Returns the full URL string

@example const char* url = webui_get_url(myWindow);"
  (window size-t))

(defcfun "webui_set_public" :void
  "@brief Allow a specific window address to be accessible from a public network

@param window The window number
@param status True or False

@example webui_set_public(myWindow, true);"
  (window size-t)
  (status :bool))

(defcfun "webui_navigate" :void
  "@brief Navigate to a specific URL

@param window The window number
@param url Full HTTP URL

@example webui_navigate(myWindow, \"http://domain.com\");"
  (window size-t)
  (url :string))

(defcfun "webui_clean" :void
  "@brief Free all memory resources. Should be called only at the end.

@example
webui_wait();
webui_clean();")

(defcfun "webui_delete_all_profiles" :void
  "@brief Delete all local web-browser profiles folder. It should called at the
end.

@example
webui_wait();
webui_delete_all_profiles();
webui_clean();")

(defcfun "webui_delete_profile" :void
  "@brief Delete a specific window web-browser local folder profile.

@param window The window number

@example
webui_wait();
webui_delete_profile(myWindow);
webui_clean();

@note This can break functionality of other windows if using the same
web-browser."
  (window size-t))

(defcfun "webui_get_parent_process_id" size-t
  "@brief Get the ID of the parent process (The web browser may re-create
another new process).

@param window The window number

@return Returns the the parent process id as integer

@example size_t id = webui_get_parent_process_id(myWindow);"
  (window size-t))

(defcfun "webui_get_child_process_id" size-t
  "@brief Get the ID of the last child process.

@param window The window number

@return Returns the the child process id as integer

@example size_t id = webui_get_child_process_id(myWindow);"
  (window size-t))

(defcfun "webui_set_port" :bool
  "@brief Set a custom web-server network port to be used by WebUI.
This can be useful to determine the HTTP link of `webui.js` in case
you are trying to use WebUI with an external web-server like NGNIX

@param window The window number
@param port The web-server network port WebUI should use

@return Returns True if the port is free and usable by WebUI

@example bool ret = webui_set_port(myWindow, 8080);"
  (window size-t)
  (port size-t))

(defcfun "webui_config" :void
  "@brief Control the WebUI behaviour. It's better to call at the beginning.

@param option The desired option from `webui_configs` enum
@param status The status of the option, `true` or `false`

@example webui_config(show_wait_connection, false);"
  (option :int)
  (status :bool))

(defcfun "webui_set_tls_certificate" :bool
  "@brief Set the SSL/TLS certificate and the private key content, both in PEM
format. This works only with `webui-2-secure` library. If set empty WebUI
will generate a self-signed certificate.

@param certificate_pem The SSL/TLS certificate content in PEM format
@param private_key_pem The private key content in PEM format

@return Returns True if the certificate and the key are valid.

@example bool ret = webui_set_tls_certificate(\"-----BEGIN
 CERTIFICATE-----\n...\", \"-----BEGIN PRIVATE KEY-----\n...\");"
  (cert-pem :string)
  (private-key-pem :string))

(defcfun "webui_run" :void
  "@brief Run JavaScript without waiting for the response.

@param window The window number
@param script The JavaScript to be run

@example webui_run(myWindow, \"alert('Hello');\");"
  (window size-t)
  (script :string))

;; TODO
(defun webui-script (window script timeout &key (max-len 1024))
  "@brief Run JavaScript and get the response back.
 Make sure your local buffer can hold the response.

 @param window The window number
 @param script The JavaScript to be run
 @param timeout The execution timeout
 @param buffer The local buffer to hold the response
 @param buffer_length The local buffer size

 @return Returns True if there is no execution error

 @example bool err = webui_script(myWindow, \"return 4 + 6;\", 0, myBuffer, myBufferSize);"
  (with-foreign-pointer-as-string (result max-len)
    (foreign-funcall "webui_script"
                     size-t window
                     :string script
                     size-t timeout
                     :pointer result
                     size-t max-len
                     :bool)))

(defcfun "webui_set_runtime" :void
  "@brief Chose between Deno and Nodejs as runtime for .js and .ts files.

  @param window The window number
  @param runtime Deno | Nodejs

@example webui_set_runtime(myWindow, Deno);"
  (window size-t)
  (runtime size-t))

(defcfun "webui_get_count" size-t
  "@brief Get how many arguments there are in an event.

@param e The event struct

@return Returns the arguments count.

@example size_t count = webui_get_count(e);"
  (event :pointer))

(defcfun "webui_get_int_at" :uint64
  "@brief Get an argument as integer at a specific index

@param e The event struct
@param index The argument position starting from 0

@return Returns argument as integer

@example long long int myNum = webui_get_int_at(e, 0);"
  (event :pointer)
  (index size-t))

(defcfun "webui_get_int" :uint64
  "@brief Get the first argument as integer

@param e The event struct

@return Returns argument as integer

@example long long int myNum = webui_get_int(e);"
  (event :pointer))

(defcfun "webui_get_float_at" :float
  "@brief Get an argument as float at a specific index

@param e The event struct
@param index The argument position starting from 0

@return Returns argument as float

@example double myNum = webui_get_float_at(e, 0);"
  (event :pointer)
  (index size-t))

(defcfun "webui_get_float" :float
  "@brief Get the first argument as float

@param e The event struct

@return Returns argument as float

@example double myNum = webui_get_float(e);"
  (event :pointer))

(defcfun "webui_get_string_at" :string
  "@brief Get an argument as string at a specific index

@param e The event struct
@param index The argument position starting from 0

@return Returns argument as string

@example const char* myStr = webui_get_string_at(e, 0);"
  (event :pointer)
  (index size-t))

(defcfun "webui_get_string" :string
  "@brief Get the first argument as string

@param e The event struct

@return Returns argument as string

@example const char* myStr = webui_get_string(e);"
  (event :pointer))

(defcfun "webui_get_bool_at" :bool
  "@brief Get an argument as boolean at a specific index

@param e The event struct
@param index The argument position starting from 0

@return Returns argument as boolean

@example bool myBool = webui_get_bool_at(e, 0);"
  (event :pointer)
  (index size-t))

(defcfun "webui_get_bool" :bool
  "@brief Get the first argument as boolean

@param e The event struct

@return Returns argument as boolean

@example bool myBool = webui_get_bool(e);"
  (event :pointer))

(defcfun "webui_get_size_at" size-t
  "@brief Get the size in bytes of an argument at a specific index

@param e The event struct
@param index The argument position starting from 0

@return Returns size in bytes

@example size_t argLen = webui_get_size_at(e, 0);"
  (event :pointer)
  (index size-t))

(defcfun "webui_get_size" size-t
  "@brief Get size in bytes of the first argument

@param e The event struct

@return Returns size in bytes

@example size_t argLen = webui_get_size(e);"
  (event :pointer))

(defcfun "webui_return_int" :void
  "@brief Return the response to JavaScript as integer.

@param e The event struct
@param n The integer to be send to JavaScript

@example webui_return_int(e, 123);"
  (event :pointer)
  (n :uint64))

(defcfun "webui_return_float" :void
  "@brief Return the response to JavaScript as float.

@param e The event struct
@param n The float to be send to JavaScript

@example webui_return_float(e, 123.321);"
  (event :pointer)
  (f :float))

(defcfun "webui_return_string" :void
  "@brief Return the response to JavaScript as string.

@param e The event struct
@param n The string to be send to JavaScript

@example webui_return_string(e, \"Response...\");"
  (event :pointer)
  (str :string))

(defcfun "webui_return_bool" :void
  "@brief Return the response to JavaScript as boolean.

@param e The event struct
@param n The boolean to be send to JavaScript

@example webui_return_bool(e, true);"
  (event :pointer)
  (bool :bool))

(defcfun "webui_interface_bind" size-t
  "@brief Bind a specific HTML element click event with a function. Empty element means all events.

@param window The window number
@param element The element ID
@param func The callback as myFunc(Window, EventType, Element, EventNumber, BindID)

@return Returns unique bind ID

@example size_t id = webui_interface_bind(myWindow, \"myID\", myCallback);"
  (window size-t)
  (element :string)
  (func :pointer))

(defcfun "webui_interface_set_response" :void
  "@brief When using `webui_interface_bind()`, you may need this function to easily set a response.

@param window The window number
@param event_number The event number
@param response The response as string to be send to JavaScript

@example webui_interface_set_response(myWindow, e->event_number, \"Response...\");"
  (window size-t)
  (event-number size-t)
  (response :string))

(defcfun "webui_interface_is_app_running" :bool
  "@brief Check if the app still running.

@return Returns True if app is running

@example bool status = webui_interface_is_app_running();")

(defcfun "webui_interface_get_window_id" size-t
  "@brief Get a unique window ID.

@param window The window number

@return Returns the unique window ID as integer

@example size_t id = webui_interface_get_window_id(myWindow);"
  (window size-t))

(defcfun "webui_interface_get_string_at" :string
  "@brief Get an argument as string at a specific index

@param window The window number
@param event_number The event number
@param index The argument position

@return Returns argument as string

@example const char* myStr = webui_interface_get_string_at(myWindow, e->event_number, 0);"
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_get_int_at" :uint64
  "@brief Get an argument as integer at a specific index

@param window The window number
@param event_number The event number
@param index The argument position

@return Returns argument as integer

@example long long int myNum = webui_interface_get_int_at(myWindow, e->event_number, 0);"
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_get_bool_at" :bool
  "@brief Get an argument as boolean at a specific index

@param window The window number
@param event_number The event number
@param index The argument position

@return Returns argument as boolean

@example bool myBool = webui_interface_get_bool_at(myWindow, e->event_number, 0);"
  (window size-t)
  (event-number size-t)
  (index size-t))

(defcfun "webui_interface_get_size_at" size-t
  "@brief Get the size in bytes of an argument at a specific index

@param window The window number
@param event_number The event number
@param index The argument position

@return Returns size in bytes

@example size_t argLen = webui_interface_get_size_at(myWindow, e->event_number, 0);"
  (window size-t)
  (event-number size-t)
  (index size-t))
