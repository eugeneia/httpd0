;;;; System definition for HTTPD0.

(defpackage httpd0-asd
  (:documentation
   "System definition for HTTPD0.")
  (:use :cl :asdf))

(in-package :httpd0-asd)

(defsystem httpd0
  :description
  "HTTP/1.0 GET/HEAD server."
  :version "1.2"
  :license "GNU AGPL"
  :author "Max Rottenkolber <max@mr.gy>"
  :components ((:file "headers")
               (:file "parse-request"
                      :depends-on ("headers"))
	       (:file "responses"
                      :depends-on ("headers"))
	       (:file "resource-responder"
		      :depends-on ("responses"))
	       (:file "router"
		      :depends-on ("responses"))
	       (:file "httpd"
		      :depends-on ("responses"
				   "parse-request"
				   "resource-responder")))
  :depends-on ("q-thread-pool"
	       "trivial-utf-8"
               "cl-date-time-parser"
	       "maxpc"
	       "percent-encoding"
	       "cl-fad"
	       "file-types"
               "uiop"))
