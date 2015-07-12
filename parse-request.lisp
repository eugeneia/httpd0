;;;; Parse HTTP/1.0 GET and HEAD requests.

(defpackage httpd0.parse-request
  (:documentation
   "Parse HTTP/1.0 GET and HEAD requests.")
  (:use :cl
	:mpc
	:mpc.characters
	:mpc.numerals
	:percent-encoding
        :cl-date-time-parser)
  (:export :parse-request))

(in-package :httpd0.parse-request)

(defun =token (token object)
  "Parse TOKEN as OBJECT."
  (=and token (=result object)))

(defun =method-token (symbol)
  "Parse method prefix for SYMBOL."
  (=token (=and (=string (symbol-name symbol) nil)
		(=character #\Space))
	  symbol))

(defun =method ()
  "Parser for request method."
  (=or (=method-token :get)
       (=method-token :head)))

(defun =host ()
  "Parser for resource host."
  (=and (=string "http://" nil)
	(=one-or-more (=not (=or (=whitespace)
				 (=character #\/))))))

(defun =resource-path ()
  "Paser for resource path."
  (=prog1 (=string-of (=not (=whitespace)))
	  (=maybe (=character #\Space))))

(defun strip-query (resource-string)
  "Strip query from RESOURCE-STRING."
  (subseq resource-string 0 (position #\? resource-string)))

(defun strip-root (resource-string)
  "Strip root from RESOURCE-STRING."
  (string-left-trim '(#\/) resource-string))

(defun decode-resource (resource-string)
  "Decode RESOURCE-STRING."
  (handler-case (pathname
                 (decode (strip-query (strip-root resource-string))
                         :www-form nil
                         :encoding :utf-8))
    ;; Yield nil when RESOURCE-STRING can not be converted to a
    ;; pathname.
    (error () nil)))

(defun =resource ()
  "Parser for requested resource."
  (=let* ((_ (=maybe (=host)))
          (resource (=resource-path)))
    (=result (decode-resource resource))))

(defun =endline ()
  "Parser for HTTP CRLF."
  (=or (=and (=character #\Return)
	     (=character #\Newline))
       (=character #\Return)
       (=character #\Newline)
       (=end-of-input)))

(defun =http-version ()
  "Parse for HTTP version strings."
  (let ((number (=one-or-more (=digit))))
    (=and (=string "HTTP/" nil)
	  number
	  (=character #\.)
	  number)))

(defun =version ()
  "Parser for request version."
  (=or (=token (=and (=http-version)
		     (=endline))
	       :1.0)
       (=token (=endline)
	       :0.9)))

(defun =header ()
  "Parse for HTTP header."
  (let ((seperator (=character #\:)))
    (=list (=prog1 (=string-of (=not (=or (=whitespace)
					  seperator)))
		   seperator
		   (=zero-or-more (=whitespace)))
	   (=prog1 (=string-of (=not (=endline)))
		   (=endline)))))

(defun =if-modified-since ()
  "Parser for IF-MODIFIED-SINCE header."
  (=let* ((headers (=zero-or-more (=header))))
    (=result (let ((if-modified-since
                    (cadr (assoc "IF-MODIFIED-SINCE" headers
                                 :test #'string-equal))))
               (and if-modified-since
                    (parse-date-time if-modified-since))))))

(defun =request ()
  "Parser for request."
  (=list (=method)
	 (=resource)
	 (=version)
	 (=maybe (=if-modified-since))))

(defun parse-request (request)
  "Parse REQUEST and return request method, resource, protocol version
  and if applicable the value of the IF-MODIFIED-SINCE header."
  (values-list (run (=request) request)))
