;;;; Headers structure.

(defpackage httpd0.headers
  (:documentation
   "Structrue for supported headers.")
  (:use :cl :cl-date-time-parser)
  (:export :make-headers
           :if-modified-since
           :content-length))

(in-package :httpd0.headers)

(defstruct (headers :conc-name)
  (if-modified-since nil)
  (content-length 0))
