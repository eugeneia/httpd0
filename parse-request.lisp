;;;; Parse HTTP/1.0 GET and HEAD requests.

(defpackage httpd0.parse-request
  (:documentation
   "Parse HTTP/1.0 GET and HEAD requests.")
  (:use :cl
	:maxpc
	:maxpc.char
	:maxpc.digit
	:percent-encoding
        :cl-date-time-parser)
  (:export :parse-request))

(in-package :httpd0.parse-request)

(defun =token (token object)
  "Parse TOKEN as OBJECT."
  (=transform token (constantly object)))

(defun =method-token (symbol)
  "Parse method prefix for SYMBOL."
  (=token (?list (?string (symbol-name symbol) nil)
                 (?char #\Space))
	  symbol))

(defun =method ()
  "Parser for request method."
  (%or (=method-token :get)
       (=method-token :head)))

(defun ?host ()
  "Parser for resource host."
  (?list (?string "http://" nil)
         (%some (?not (%or (?whitespace) (?char #\/))))))

(defun =resource-path ()
  "Paser for resource path."
  (=destructure (path _)
      (=list (=subseq (%any (?not (?whitespace))))
             (%maybe (?char #\Space)))))

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
  (=destructure (_ resource)
      (=list (%maybe (?host)) (=resource-path))
    (decode-resource resource)))

(defun ?endline ()
  "Parser for HTTP CRLF."
  (%or (?list (?char #\Return)
              (?char #\Newline))
       (?char #\Return)
       (?char #\Newline)
       (?end)))

(defun ?http-version ()
  "Parse for HTTP version strings."
  (?list (?string  "HTTP/" nil)
         (%some (?digit))
         (?char #\.)
         (%some (?digit))))

(defun =version ()
  "Parser for request version."
  (%or (=token (?list (?http-version) (?endline))
	       :1.0)
       (=token (?endline)
	       :0.9)))

(defun =header ()
  "Parser for HTTP header."
  (let ((separator (?char #\:)))
    (=destructure (key _ _ value _)
        (=list (=subseq (%some (?not (%or (?whitespace) separator))))
               separator
               (%any (%diff (?whitespace) (?newline) (?char #\Return)))
               (=subseq (%any (?not (?endline))))
               (?endline))
      (cons key value))))

(defun =if-modified-since ()
  "Parser for IF-MODIFIED-SINCE header."
  (=transform (%any (=header))
              (lambda (headers)
                (let ((if-modified-since
                       (cdr (assoc "IF-MODIFIED-SINCE" headers
                                   :test #'string-equal))))
                  (and if-modified-since
                       (parse-date-time if-modified-since))))))

(defun =request ()
  "Parser for request."
  (=list (=method)
	 (=resource)
	 (=version)
	 (%maybe (=if-modified-since))))

(defun parse-request (request)
  "Parse REQUEST and return request method, resource, protocol version
  and if applicable the value of the IF-MODIFIED-SINCE header."
  (values-list (parse request (=request))))
