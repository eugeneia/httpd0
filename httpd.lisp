;;;; HTTP/1.0 GET/HEAD server.

(defpackage httpd0
  (:documentation "HTTP/1.0 GET/HEAD server.")
  (:use :cl
	:q-thread-pool
	:usocket
	:trivial-utf-8
	:httpd0.parse-request
	:httpd0.responses
	:httpd0.resource-responder)
  (:export :*request-size*
	   :*request-timeout*
	   :make-httpd
	   :destroy-httpd
	   :make-resource-responder))

(in-package :httpd0)

(defparameter *request-size* 512
  "Maxmum size of a request.")

(defparameter *request-timeout* 64
  "Timeout for clients to submit request.")

(defparameter *return-code* (char-code #\Return)
  "ASCII code for #\Return.")

(defparameter *newline-code* (char-code #\Newline)
  "ASCII code for #\Newline.")

(defun request-complete-p (request-buffer pos)
  "Predicate to test if REQUEST-BUFFER contains a complete request."
  ;; WARNING: Most ridiculous piece of code ever...
  (or (and (> pos 2)
	   (= *newline-code*
	      (aref request-buffer pos)         ; X  X  X  LF
	      (aref request-buffer (- pos 2)))  ; X  LF X  X
	   (= *return-code*
	      (aref request-buffer (1- pos))    ; X  X  CR X
	      (aref request-buffer (- pos 3)))) ; CR X  X  X
      (and (> pos 0)
	   (or (= *newline-code*
		  (aref request-buffer pos)       ; X  LF
		  (aref request-buffer (1- pos))) ; LF X
	       (= *return-code*
		  (aref request-buffer pos)           ; X  LF
		  (aref request-buffer (1- pos))))))) ; LF X

(defun timeout-p (connection)
  "Predicate to test if CONNECTION timed out."
  (not (nth-value 1 (wait-for-input connection
				    :timeout *request-timeout*
				    :ready-only nil))))

(defun receive-request (connection)
  "Return request buffer received from CONNECTION with its input stream
bound to *STANDARD-INPUT* or NIL if *REQUEST-SIZE* or *REQUEST-TIMEOUT*
is exceeded."
  (let ((buffer (make-array *request-size*
			    :element-type '(unsigned-byte 8))))
    (loop for i from 0 to *request-size*
       if (or (= *request-size* i)
	      (timeout-p connection))
       return nil
       else do (let ((byte (read-byte *standard-input* nil 'eof)))
		 (if (eq 'eof byte)
		     (return-from receive-request (subseq buffer 0 i))
		     (setf (aref buffer i) byte)))
       when (request-complete-p buffer i)
       return (subseq buffer 0 (1+ i)))))

(defun request-buffer-to-string (request-buffer)
  "Convert REQUEST-BUFFER to string or return NIL if unable to."
  (handler-case (utf-8-bytes-to-string request-buffer)
    (utf-8-decoding-error () nil)))

(defun read-request (connection)
  "Return request from CONNECTION or NIL if its invalid or timed out."
  (let ((request-buffer (receive-request connection)))
    (when request-buffer
      (request-buffer-to-string request-buffer))))

(defun respond (connection responder)
  "Respond to CONNECTION using RESPONDER."
  (let ((request (read-request connection)))
    (when request
      (multiple-value-bind (*request-method*
			    resource
			    *protocol-version*
			    if-modified-since)
	  (parse-request request)
	(if (and *protocol-version* *request-method*)
            (if resource
                (funcall responder resource if-modified-since)
                (respond-not-found))
	    (respond-not-implemented))))))

(defmacro handle-errors (&body body)
  "Handle errors in BODY."
  `(handler-case ,@body
     (error (error) (format *error-output* "~s ~a~%" error error))))

(defun accept (socket responder thread-pool)
  "Accept connection from SOCKET and respond using RESPONDER in
THREAD-POOL."
  (let ((connection
	 (socket-accept socket :element-type '(unsigned-byte 8))))
    (enqueue-task thread-pool
      (handle-errors
       (let ((*standard-input* (socket-stream connection))
	     (*standard-output* (socket-stream connection)))
	 (unwind-protect (respond connection responder)
	   (socket-close connection)))))))


(defun make-server (host port socket-backlog responder thread-pool)
  "Make server listening on HOST and PORT with SOCKET-BACKLOG using
RESPONDER in THREAD-POOL."
  (let ((socket (socket-listen host port
			       :reuse-address t
			       :backlog socket-backlog
			       :element-type '(unsigned-byte 8))))
    (unwind-protect (loop do (handle-errors
                              (accept socket responder thread-pool)))
      (socket-close socket))))

(defun make-httpd (responder &key (host *wildcard-host*)
                                  (port 8080)
			          (n-threads 16)
		                  (socket-backlog 32))
  "Make HTTP server which listens on HOST and PORT with N-THREADS and
responds with RESPONDER. Unless a HOST is specified, the daemon will
listen on all adresses. PORT defaults to 8080, N-THREADS to 16 and
SOCKET-BACKLOG to 32." 
  (let ((thread-pool (make-thread-pool n-threads (* 4 n-threads))))
    (enqueue-task thread-pool
      (make-server host port socket-backlog responder thread-pool))
    thread-pool))

(defun destroy-httpd (httpd)
  "Destroys threads used by HTTPD."
  (destroy-thread-pool httpd))
