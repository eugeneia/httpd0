;;;; HTTPD0 resource responder.

(defpackage httpd0.resource-responder
  (:documentation
   "HTTPD0 resource responder.")
  (:use :cl
        :cl-fad
        :trivial-utf-8
        :file-types
        :httpd0.responses)
  (:import-from :uiop :native-namestring)
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
        (if (directory-pathname-p path)
            (if (directory-pathname-p request)
                (values :ok (or (directory-index path) path))
                (values :redirect (pathname-as-directory
                                   (merge-pathnames request #p"/"))))
            (values :ok path))))))

(defun last-directory (path)
  "Return pathname for relative last directory of PATH."
  (let ((last-directory (car (last (pathname-directory path)))))
    (make-pathname :directory `(:relative ,last-directory))))

(defun entry-name (entry)
  "Returns name for directory ENTRY."
  (native-namestring (if (directory-pathname-p entry)
                         (directory-namestring (last-directory entry))
                         (file-namestring entry))))

(defun serve-file (path write-date)
  "Serve file at PATH with WRITE-DATE."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (respond-ok ((file-length in) (file-mime path) write-date)
      (copy-stream in *standard-output* nil))))

(defun directory-listing (path)
  "Create directory listing for PATH."
  (string-to-utf-8-bytes
   (with-output-to-string (out)
     (dolist (entry (list-directory path :follow-symlinks nil))
       (format out "~a~%" (entry-name entry))))))

(defun serve-directory (path write-date)
  "Serve directory at PATH with WRITE-DATE."
  (let ((listing (directory-listing path)))
    (respond-ok ((length listing) *text-mime* write-date)
		(write-sequence listing *standard-output*))))

(defun serve (resource if-modified-since)
  "Serve RESOURCE unless it has not been modified since
 IF-MODIFIED-SINCE."
  (let ((write-date (file-write-date resource)))
    (if (and if-modified-since
             (>= if-modified-since write-date))
        (respond-not-modified)
        (if (directory-pathname-p resource)
            (serve-directory resource write-date)
            (serve-file resource write-date)))))

(defun make-resource-responder (root)
  "*Arguments and Values:*

   _root_—a _directory pathname_.

   *Description:*

   {make-resource-responder} returns a _responder function_ that serves
   the files under _root_."
  (lambda (resource parameters headers)
    (declare (ignore parameters))
    (if (eq *request-method* :post)
        (respond-not-implemented)
        (multiple-value-bind (status path)
            (locate-resource resource root)
          (case status
            (:ok (serve path (if-modified-since headers)))
            (:redirect (respond-moved-permanently (native-namestring path)))
            (otherwise (respond-not-found)))))))
