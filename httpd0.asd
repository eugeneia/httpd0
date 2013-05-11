;;;; System definition for HTTPD0.

(defpackage httpd0-asd
  (:documentation
   "System definition for HTTPD0.")
  (:use :cl :asdf))

(in-package :httpd0-asd)

(defsystem httpd0
  :description
  "HTTP/1.0 GET/HEAD server."
  :components ((:file "parse-request")
	       (:file "responses")
	       (:file "resource-responder"
		      :depends-on ("responses"))
	       (:file "httpd"
		      :depends-on ("responses"
				   "parse-request"
				   "resource-responder")))
  :depends-on ("q-thread-pool"
	       "usocket"
	       "trivial-utf-8"
	       "net-telent-date"
	       "smug"
	       "percent-encoding"
	       "cl-fad"
	       "file-types"
               "pretty-string"))
