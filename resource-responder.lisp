;;;; HTTPD0 resource responder.

(defpackage httpd0.resource-responder
  (:documentation
   "HTTPD0 resource responder.")
  (:use :cl
	:cl-fad
	:trivial-utf-8
	:file-types
        :pretty-string
	:httpd0.responses)
  (:export :make-resource-responder))

(in-package :httpd0.resource-responder)

(defun valid-request-p (request)
  "Predicate to check if REQUEST should honored."
  (not (member :up (pathname-directory request))))

(defun directory-index (path)
  "Return index for directory at PATH or NIL."
  (first (directory (merge-pathnames #p"index.*" path))))

(defun locate-resource (request root)
  "Return resource for REQUEST in ROOT."
  (when (valid-request-p request)
    (let ((path (probe-file (merge-pathnames request root))))
      (when path
	(or (when (directory-pathname-p path)
	      (directory-index path))
	    path)))))

(defun entry-name (entry)
  "Returns name for directory ENTRY."
  (native-namestring (if (directory-pathname-p entry)
                         (car (last (pathname-directory entry)))
                         (file-namestring entry))))

(defun serve-file (path write-date)
  "Serve file at PATH with WRITE-DATE."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (respond-ok ((file-length in) (file-mime path) write-date)
      (copy-stream in *standard-output*))))

(defun directory-listing (path)
  "Create directory listing for PATH."
  (string-to-utf-8-bytes
   (with-output-to-string (out)
     (dolist (entry (list-directory path))
       (format out "~a~%" (entry-name entry))))))

(defun serve-directory (path write-date)
  "Serve directory at PATH with WRITE-DATE."
  (let ((listing (directory-listing path)))
    (respond-ok ((length listing) *text-mime* write-date)
		(write-sequence listing *standard-output*))))

(defun serve (resource if-modified-since)
  "Serve RESOURCE unless it has not been modified since
 IF-MODIFIED-SINCE."
  (if resource
      (let ((write-date (file-write-date resource)))
	(if (and if-modified-since
		 (>= if-modified-since write-date))
	    (respond-not-modified)
	    (if (directory-pathname-p resource)
		(serve-directory resource write-date)
		(serve-file resource write-date))))
      (respond-not-found)))

(defun make-resource-responder (root)
  "Return resource responder for ROOT."
  (lambda (resource if-modified-since)
    (serve (locate-resource resource root) if-modified-since)))
