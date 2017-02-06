;;;; Parse HTTP/1.0 GET and HEAD requests.

(defpackage httpd0.parse-request
  (:documentation
   "Parse HTTP/1.0 GET and HEAD requests.")
  (:use :cl
        :httpd0.headers
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
  (=token (?seq (?string (symbol-name symbol) nil)
                (?char #\Space))
	  symbol))

(defun =method ()
  "Parser for request method."
  (%or (=method-token :get)
       (=method-token :head)
       (=method-token :post)))

(defun ?host ()
  "Parser for resource host."
  (?seq (?string "http://" nil)
        (%some (?not (%or (?whitespace) (?char #\/))))))

(defun ?query-delimiter ()
  "Parser for request query parameters delimiter."
  (?char #\?))

(defun =resource-path ()
  "Paser for resource path."
  (=subseq (%any (?not (%or (?whitespace)
                            (?query-delimiter))))))

(defun strip-root (resource-string)
  "Strip root from RESOURCE-STRING."
  (string-left-trim '(#\/) resource-string))

(defun decode-resource (resource-string)
  "Decode RESOURCE-STRING."
  (handler-case (pathname (decode (strip-root resource-string)
                                  :www-form nil
                                  :encoding :utf-8))
    ;; Yield nil when RESOURCE-STRING can not be converted to a
    ;; pathname.
    (error () nil)))

(defun =resource ()
  "Parser for requested resource."
  (=destructure (_ resource _)
      (=list (%maybe (?host))
             (=resource-path)
             (%maybe (?char #\Space)))
    (decode-resource resource)))

(defun =parameter ()
  "Parser for query parameter."
  (=destructure (key value _)
      (=list
       ;; KEY is is a string terminated by #\&, #\= or whitespace.
       (=subseq (%some (?not (%or (?test ('member '(#\& #\=)))
                                  (?whitespace)))))
       ;; VALUE is optional, it can be either #\= followed by a string
       ;; terminated by #\& or whitespace, just #\= by itself or nothing at
       ;; all.
       (%or
        ;; VALUE will be the string after #\=.
        (=destructure (_ value)
            (=list (?char #\=)
                   (=subseq (%some (?not (%or (?char #\&)
                                              (?whitespace)))))))
        ;; VALUE will be NIL.
        (%maybe (?char #\=)))
       ;; Consume terminating #\& if present.
       (%maybe (?char #\&)))
    ;; Return KEY and VALUE as a cons.
    (cons key value)))

(defun =parameters ()
  "Parser for query parameters."
  (=destructure (_ parameters _)
      (=list (?query-delimiter)
             (%some (=parameter)) ; Assoc-list of parameters.
             (%maybe (?char #\Space)))))

(defun ?endline ()
  "Parser for HTTP CRLF."
  (%or (?seq (?char #\Return) (?char #\Newline))
       (?char #\Return)
       (?char #\Newline)
       (?end)))

(defun ?http-version ()
  "Parse for HTTP version strings."
  (?seq (?string  "HTTP/" nil)
        (%some (?digit))
        (?char #\.)
        (%some (?digit))))

(defun =version ()
  "Parser for request version."
  (%or (=token (?seq (?http-version) (?endline))
	       :1.0)
       (=token (?endline)
	       :0.9)))


(defun =header (&optional key)
  "Parser for HTTP header."
  (let ((separator (?char #\:)))
    (=destructure (_ _ _ value _)
        (=list (or key (%some (?not (%or (?whitespace) separator))))
               separator
               (%any (%diff (?whitespace) (?newline) (?char #\Return)))
               (=subseq (%any (?not (?endline))))
               (?endline)))))

(defun !if-modified-since (headers)
  (=transform (=header (?string "If-Modified-Since" nil))
              (lambda (value)
                (setf (if-modified-since headers)
                      (ignore-errors (parse-date-time value))))))

(defun !content-length (headers)
  (=transform (=header (?string "Content-Length" nil))
              (lambda (value)
                (setf (content-length headers)
                      (parse-integer value)))))

(defun =headers (headers)
  (=destructure () (%any (?seq (%or (!if-modified-since headers)
                                    (!content-length headers)
                                    (=header))))
    headers))

(defun =request ()
  "Parser for request."
  (=list (=method) (=resource) (%maybe (=parameters)) (=version)
         (=headers (make-headers))))

(defun parse-request (stream request-size)
  "Parse REQUEST and return request method, resource, protocol version
  and if applicable the value of the IF-MODIFIED-SINCE header."
  (let ((maxpc.input.stream:*chunk-size* request-size)
        (maxpc.input.stream:*bound* request-size))
    (values-list (parse stream (=request)))))
