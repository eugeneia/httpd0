;;;; HTTP/1.0 GET/HEAD server.

(defpackage httpd0
  (:documentation
   "HTTP/1.0 GET/HEAD server. Server responses are generated by functions
    implementing the _responder function_ interface:

    — Function: *<responder>* _resource_ _if-modified-since_

    *Arguments and Values:*

    _resource_—a _pathname_.

    _if-modified-since_—the value of the _If-Modified-Since_ header
    represented as a _universal time_.

    *Description:*

    A _responder function_ must produce a response to the request to
    _resource_ using the helper functions provided by the
    {httpd0.responses} _package_.

    *See Also:*

    + [If-Modified-Since](http://www.w3.org/Protocols/HTTP/1.0/spec.html#If-Modified-Since)
    + httpd0.responses")
  (:use :cl
	:ccl
        :q-thread-pool
	:httpd0.parse-request
	:httpd0.responses
	:httpd0.resource-responder)
  (:export :http-respond
           :*request-size*
	   :*request-timeout*
	   :make-httpd
	   :destroy-httpd
	   :make-resource-responder))

(in-package :httpd0)

(defparameter *request-size* 512
  "*Description:*

   Maximum request size in characters. Requests exceeding {*request-size*} are
   dropped by closing the connection.")

(defparameter *request-timeout* 64
  "*Description:*

   I/O timeout in seconds. Requests that are stalled by an I/O operation with
   the client for more than the specified duration are dropped by closing the
   connection.")

(defun http-respond (connection responder)
  "Respond to CONNECTION using RESPONDER."
  (let ((*standard-input* connection)
        (*standard-output* connection))
    (multiple-value-bind (*request-method* resource *protocol-version* headers)
        (parse-request *standard-input* *request-size*)
      (if *protocol-version*
          (if resource
              (funcall responder resource headers)
              (respond-not-found))
          (respond-not-implemented)))))

(defmacro handle-errors (&body body)
  "Handle errors in BODY."
  `(handler-case ,@body
     (error (error) (format *error-output* "~s ~a~%" error error))))

(defun accept (socket responder thread-pool)
  "Accept connection from SOCKET and respond using RESPONDER in
THREAD-POOL."
  (let ((connection (accept-connection socket)))
    (enqueue-task thread-pool
      (handle-errors (unwind-protect (http-respond connection responder)
                       (close connection))))))

(defun make-server (host port socket-backlog responder thread-pool)
  "Make server listening on HOST and PORT with SOCKET-BACKLOG using
RESPONDER in THREAD-POOL."
  (let ((socket (make-socket :connect :passive
                             :local-host host
                             :local-port port
                             :backlog socket-backlog
                             :external-format :utf-8
                             :input-timeout *request-timeout*
                             :output-timeout *request-timeout*)))
    (unwind-protect
         (loop do (handle-errors (accept socket responder thread-pool)))
      (close socket))))

(defun make-httpd (responder &key host
                                  (port 8080)
			          (n-threads 16)
		                  (socket-backlog 32))
  "*Arguments and Values:*

   _responder_—a _responder function_.

   _host_—the _host address_ to listen on as a _string_. The default is the
   wildcard address.

   _port_—a local port number. The default is 8080.

   _n-threads_—a positive _integer_ specifying the number of threads to
   keep in the thread pool. Must be at least two. The default is 16.

   _socket-backlog_—a positive _integer_ specifying the socket _backlog_. The
   default is 32.

   *Description:*

   {make-httpd} creates a httpd0 server instance with _responder_ that
   listens on the specified _host_ and _port_."
  (let ((thread-pool (make-thread-pool n-threads (* 4 n-threads))))
    (enqueue-task thread-pool
      (make-server host port socket-backlog responder thread-pool))
    thread-pool))

(defun destroy-httpd (httpd)
  "*Arguments and Values:*

   _httpd_—an httpd0 instance as returned by {make-httpd}.

  *Description:*

  {destory-httpd} stops _httpd_ and frees its resources."
  (destroy-thread-pool httpd))
