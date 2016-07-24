;;;; Parse HTTP/1.0 GET and HEAD requests.

(defpackage httpd0.parse-request
  (:documentation
   "Parse HTTP/1.0 GET and HEAD requests.")
  (:use :cl
	:maxpc
        :maxpc.char
        :trivial-utf-8
	:percent-encoding
        :cl-date-time-parser)
  (:export :parse-request))

(in-package :httpd0.parse-request)

(defun =token (token object)
  "Parse TOKEN as OBJECT."
  (=transform token (constantly object)))

(defun ?seq (s &rest rest)
  (apply '?list
         (loop for i from 0 to (1- (length s)) collect
              (apply '%or
                     (?eq (aref s i))
                     (loop for o in rest collect
                          (?eq (aref o i)))))))

(defun ?useq (&rest str)
  (apply '?seq (loop for s in str collect (string-to-utf-8-bytes s))))

(defun ?uchar (c)
  (?useq (make-array 1 :element-type 'character :initial-element c)))

(defun ?uwhitespace ()
  (apply '%or (loop for w in *whitespace* collect (?uchar w))))

(defun =method-token (symbol)
  "Parse method prefix for SYMBOL."
  (=token (?list (?useq #1=(symbol-name symbol) (string-upcase #1#))
                 (?uchar #\Space))
	  symbol))

(defun =method ()
  "Parser for request method."
  (%or (=method-token :get)
       (=method-token :head)))

(defun ?host ()
  "Parser for resource host."
  (?list (?useq "http://" "HTTP://")
         (%some (?not (%or (?uwhitespace) (?uchar #\/))))))

(defun =resource-path ()
  "Paser for resource path."
  (=destructure (path _)
      (=list (=subseq (%any (?not (?uwhitespace))))
             (%maybe (?uchar #\Space)))))

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
    (decode-resource (utf-8-bytes-to-string resource))))

(defun ?endline ()
  "Parser for HTTP CRLF."
  (let ((lf (?uchar #\Newline)) (ret (?uchar #\Return)))
    (%or (?list ret lf)
         ret
         lf
         (?end))))

(defun udigit-p (c)
  (<= #x30 c #x39))

(defun ?udigit ()
  (?satisfies 'udigit-p))

(defun ?http-version ()
  "Parse for HTTP version strings."
  (?list (?useq "HTTP/" "http/")
         (%some (?udigit))
         (?uchar #\.)
         (%some (?udigit))))

(defun =version ()
  "Parser for request version."
  (%or (=token (?list (?http-version) (?endline))
	       :1.0)
       (=token (?endline)
	       :0.9)))

(defun =header ()
  "Parser for HTTP header."
  (let ((separator (?uchar #\:)))
    (=destructure (key _ _ value _)
        (=list (=subseq (%some (?not (%or (?uwhitespace) separator))))
               separator
               (%any (%diff (?uwhitespace) (?uchar #\Newline) (?uchar #\Return)))
               (=subseq (%any (?not (?endline))))
               (?endline))
      (cons key value))))

(defun =if-modified-since ()
  "Parser for IF-MODIFIED-SINCE header."
  (=destructure (&rest headers) (%any (=header))
    (let ((if-modified-since
           (cdr (assoc "IF-MODIFIED-SINCE" headers
                       :test 'string-equal
                       :key 'utf-8-bytes-to-string))))
      (when if-modified-since
        (parse-date-time (utf-8-bytes-to-string if-modified-since))))))

(defun =request ()
  "Parser for request."
  (=list (=method)
	 (=resource)
	 (=version)
	 (%maybe (=if-modified-since))))

(defun parse-request (stream bound)
  "Parse request from STREAM and return request method, resource, protocol
  version and if applicable the value of the IF-MODIFIED-SINCE header."
  (let ((maxpc.input.stream:*stream-bound* bound)
        (maxpc.input.stream:*chunk-size* bound)
        (maxpc.input.stream:*stream-element-type* '(unsigned-byte 8)))
    (values-list (parse stream (=request)))))
