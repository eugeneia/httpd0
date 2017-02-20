;;;; HTTPD0 responses.

(defpackage httpd0.responses
  (:documentation
   "Toolkit for writing _responder functions_. Includes common responses
    and generic response templates.")
  (:use :cl
        :httpd0.headers
	:trivial-utf-8
        :percent-encoding)
  (:export :*protocol-version*
	   :*request-method*
	   :*text-mime*
           :if-modified-since
           :content-length
           :uri-encode
           :universal-time-to-http-date
	   :respond-ok
           :respond-moved-permanently
	   :respond-not-modified
	   :respond-not-found
	   :respond-not-implemented))

(in-package :httpd0.responses)

(defvar *protocol-version* nil
  "*Description:*

   {*protocol-version*} is bound to a _symbol_ indicating the protocol
   version is use when calling a _responder function_.
   {*protocol-version*} can be either {:0.9} or {:1.0} to indicate
   HTTP/0.9 or HTTP/1.0 respectively.")

(defvar *request-method* nil
  "*Description:*

   {*request-method*} is bound to a _symbol_ indicating the request
   method served when calling a _responder function_. {*request-method*}
   can be either {:get}, {:head}, or {:post} indicating a GET, HEAD, or
   POST request respectively.")

(defparameter *text-mime* '("text" "plain; charset=utf-8")
  "*Description:*

   {*text-mime*} is bound to a _list_ of two _strings_ that designates a
   MIME type, to which responses for the {text/plain} MIME type will be
   upgraded to.")

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

(declaim (inline uri-encode-p))
(defun uri-encode-p (c)
  (or (unreservedp c)
      (reservedp c)))

(defun uri-encode (string)
  "*Arguments and Values:*

   _string_—a _string_.

   *Description:*

   {uri-encode} returns an URI safe copy of _string_. Note that
   {uri-encode} will *not* encode reserved characters.

   *See Also:*

   + [percent-encoding](http://tools.ietf.org/html/rfc3986#section-2.1)"
  (encode string :test 'uri-encode-p :www-form nil :encoding :utf-8))

(defun status-string (status)
  "Return string for STATUS."
  (ecase status
    (:ok                "200 OK")
    (:moved-permanently "301 MOVED PERMANENTLY")
    (:not-modified      "304 NOT MODIFIED")
    (:not-found         "404 NOT FOUND")
    (:not-implemented   "501 NOT IMPLEMENTED")))

(defun respond (status &optional headers)
  "Respond with STATUS, HEADERS and response head terminating
*RETURN-NEWLINE*."
  (write-utf-8-bytes (format nil "HTTP/1.0 ~a~a"
			     (status-string status)
			     *return-newline*)
		     *standard-output*)
  (loop for (header value) in headers
     do (write-utf-8-bytes (format nil "~a: ~a~a"
				   header
				   value
				   *return-newline*)
			   *standard-output*))
  (write-utf-8-bytes *return-newline* *standard-output*))

(defmacro response-body (&body body)
  "Execute BODY unless *REQUEST-METHOD* is :HEAD."
  `(unless (eq :head *request-method*)
     ,@body))

(defun mime-string (type subtype)
  "Returns mime string for TYPE and SUBTYPE."
  (format nil "~a/~a" type subtype))

(defun mime-string* (type subtype)
  (if (and (string= "text" type)
	   (string= "plain" subtype))
      (destructuring-bind (type subtype) *text-mime*
        (mime-string type subtype))
      (mime-string type subtype)))

(let ((days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
      (months #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
  (defun universal-time-to-http-date (universal-time)
    "*Arguments and Values:*

     _universal-time_—a _universal time_.

     *Description:*

     {universal-time-to-http-date} returns a _string_ encoding of
     _universal-time_ according to the format defined in
     [RFC 822 Section 5, “Date and Time Specification”](https://tools.ietf.org/html/rfc822#section-5).
     _Universal-time_ is always encoded as Greenwich Mean Time (GMT), and the
     resulting string is a valid
     [HTTP date](https://www.w3.org/Protocols/HTTP/1.0/spec.html#DateFormats).

     *Examples:*

     #code#
     (universal-time-to-http-date (get-universal-time))
     → \"Mon, 20 Feb 2017 17:42:12 GMT\"
     #"
    (multiple-value-bind (second minute hour date month year day
                          daylight-p zone)
        (decode-universal-time universal-time 0) ; Always GMT
      (declare (ignore daylight-p zone))
      (format nil "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT"
              (aref days day) date (aref months (1- month)) year
              hour minute second))))

(defun headers (&key length type (date (get-universal-time)) write-date
                  location)
  "Returns headers for LENGTH, TYPE, DATE, WRITE-DATE and LOCATION."
  `(,@(when length
	`((:content-length ,(write-to-string length))))
    ,@(when type
        `((:content-type ,(destructuring-bind (type subtype) type
                            (mime-string* type subtype)))))
    ,@(when date
        `((:date ,(universal-time-to-http-date date))))
    ,@(when write-date
	`((:last-modified ,(universal-time-to-http-date write-date))))
    ,@(when location
        `((:location ,location)))))

(defmacro respond-ok ((length type write-date) &body body)
  "*Arguments and Values:*

   _length_—a non-negative _integer_.

   _type_—a _list_ of two _strings_ designating a MIME type.

   _write-date_—a _universal time_.

   _body_—_forms_ that print _length_ bytes to {*standard-output*}.

   *Description:*

   {respond-ok} responds with HTTP status {:ok} for an entity of _length_
   bytes with MIME _type_ and _write-date_. The _body_ forms must write
   the contents of the HTTP entity to {*standard-output*}."
  `(progn (respond :ok
		   (headers :length ,length
			    :type ,type
			    :write-date ,write-date))
	  (response-body ,@body)))

(defun respond-moved-permanently (location)
  "*Arguments and Values:*

   _location_—a _string_ denoting a URI.

   *Description:*

   {respond-moved-permanently} responds with HTTP status
   {:moved-permanently} to _location_."
  (let ((message (string-to-utf-8-bytes
                  (format nil "Moved permanently to: ~a~%" location))))
    (respond :moved-permanently
             (headers :length (length message)
                      :location (uri-encode location)))
    (response-body (write-sequence message *standard-output*))))

(defun respond-not-modified ()
  "*Arguments and Values:*

   None.

   *Description:*

   {respond-not-modified} responds with HTTP status {:not-modified}."
  (respond :not-modified (headers)))

(defun respond-not-found ()
  "*Arguments and Values:*

   None.

   *Description:*

   {respond-not-found} responds with HTTP status {:not-found}."
  (respond :not-found
	   (headers :length *not-found-message-length*
		    :type *text-mime*))
  (response-body (write-sequence *not-found-message* *standard-output*)))

(defun respond-not-implemented ()
  "*Arguments and Values:*

   None.

   *Description:*

   {respond-not-implemented} responds with HTTP status {:not-implemented}."
  (respond :not-implemented
	   (headers :length *not-implemented-message-length*
		    :type *text-mime*
		    :date nil))
  (response-body
    (write-sequence *not-implemented-message* *standard-output*)))
