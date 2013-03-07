;;;; HTTPD0 responses.

(defpackage httpd0.responses
  (:documentation "HTTPD0 responses.")
  (:use :cl
	:trivial-utf-8
	:net.telent.date)
  (:export :*protocol-version*
	   :*request-method*
	   :*text-mime*
	   :respond-ok
	   :respond-not-modified
	   :respond-not-found
	   :respond-not-implemented))

(in-package :httpd0.responses)

(defparameter *protocol-version* :1.0
  "Protocol version can be :0.9 or :1.0.")

(defparameter *request-method* :get
  "Request method may be :GET or :HEAD.")

(defparameter *utf-8-text-mime* '("text" "plain; charset=utf-8")
  "Mime type for plain text.")

(defparameter *text-mime* *utf-8-text-mime*
  "Mime type for plain text files.")

(defparameter *not-found-message*
  (string-to-utf-8-bytes (format nil "?~%Not found~%"))
  "'Not found' message.")

(defparameter *not-found-message-length*
  (length *not-found-message*)
  "Length of 'not found' Message.")

(defparameter *not-implemented-message*
  (string-to-utf-8-bytes (format nil "?~%Not implemented~%"))
  "'Not implemented' message.")

(defparameter *not-implemented-message-length*
  (length *not-implemented-message*)
  "Length of 'not implemented' Message.")

(defparameter *return-newline* (format nil "~a~a" #\Return #\Newline)
  "Carriage return followed by Newline.")

(defun status-string (status)
  "Return string for STATUS."
  (ecase status
    (:ok              "200 OK")
    (:not-modified    "304 NOT MODIFIED")
    (:not-found       "404 NOT FOUND")
    (:not-implemented "501 NOT IMPLEMENTED")))

(defun respond (status &optional headers)
  "Respond with STATUS and HEADERS."
  (write-utf-8-bytes (format nil "HTTP/1.0 ~a~a"
			     (status-string status)
			     *return-newline*)
		     *standard-output*)
  (loop for (header value) in headers
     do (write-utf-8-bytes (format nil "~a: ~a~a"
				   header
				   value
				   *return-newline*)
			   *standard-output*)))

(defmacro response-body (&body body)
  "Execute BODY only when *REQUEST-METHOD* is :GET."
  `(when (eq :get *request-method*)
     (write-utf-8-bytes *return-newline* *standard-output*)
     ,@body))

(defun mime-string (type subtype)
  "Returns mime string for TYPE and SUBTYPE."
  (format nil "~a/~a" type subtype))

(defun mime-string* (type subtype)
  (if (and (string= "text" type)
	   (string= "plain" subtype))
      (mime-string (car *text-mime*) (cadr *text-mime*))
      (mime-string type subtype)))

(defun headers (length type &optional write-date)
  "Returns headers for LENGTH, TYPE and optionally WRITE-DATE."
  `((:content-length ,(write-to-string length))
    (:content-type ,(mime-string* (car type) (cadr type)))
    ,@(when write-date
	`((:last-modified ,(universal-time-to-http-date write-date))))))

(defmacro respond-ok ((length type write-date) &body body)
  "Respond with BODY as entity body, described by LENGTH, TYPE and
WRITE-DATE."
  `(progn (respond :ok (headers ,length ,type ,write-date))
	  (response-body ,@body)))

(defun respond-not-modified ()
  "Respond with status :NOT-MODIFIED."
  (respond :not-modified nil))

(defun respond-not-found ()
  "Respond with status :NOT-FOUND."
  (respond :not-found
	   (headers *not-found-message-length* *utf-8-text-mime*))
  (response-body
    (write-sequence *not-found-message* *standard-output*)))

(defun respond-not-implemented ()
  "Respond with status :NOT-IMPLEMENTED."
  (respond :not-implemented
	   (headers *not-implemented-message-length* *utf-8-text-mime*))
  (response-body
    (write-sequence *not-implemented-message* *standard-output*)))
