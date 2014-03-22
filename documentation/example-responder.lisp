(defpackage example-responder
  (:use :cl :httpd0.responses :trivial-utf-8)
  (:export :responder))

(in-package :example-responder)

(defun responder (resource if-modified-since)
  (declare (ignore if-modified-since)) ; We don't use it.
  (cond

    ;; If REQUEST is #p"hello/<string>" respond with "Hello <string>!".
    ((equal '(:relative "hello") (pathname-directory resource))
     (let ((response (string-to-utf-8-bytes
                      (format nil "Hello ~a!~%"
                              (pathname-name resource)))))
       (respond-ok ((length response) *text-mime* (get-universal-time))
         (write-sequence response *standard-output*))))

    ;; Otherwise respond with status code 404.
    (t (respond-not-found))))
