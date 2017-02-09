;;;; HTTP router

(defpackage httpd0.router
  (:use :cl :httpd0.responses)
  (:export :define-router))

(in-package :httpd0.router)

(defun compile-route-predicate (resource)
  (if (wild-pathname-p resource)
      `(pathname-match-p resource ,resource)
      `(equal resource ,resource)))

(defun compile-route-arguments (resource parameters headers)
  `(,@(when (wild-pathname-p resource :name)
        '((pathname-name resource)))
    ,@(when (wild-pathname-p resource :type)
        '((pathname-type resource)))
    ,@(loop for parameter in parameters collect
           `(cdr (assoc ,(symbol-name parameter) parameters
                        :test 'string-equal)))
    ,@(loop for header in headers collect
           `(,header headers))))

(defun compile-routes (routes)
  `(cond ,@(loop for (() resource function parameters headers) in routes
              collect `(,(compile-route-predicate resource)
                        (funcall ',function ,@(compile-route-arguments
                                               resource parameters headers))))
         (t (respond-not-found))))

(defun applicable-routes (method routes)
  (remove-if-not (lambda (route-methods)
                   (member method route-methods))
                 routes
                 :key 'first))

(defmacro define-router (name &body routes)
  `(defun ,name (resource parameters headers)
     (case *request-method*
       ,@(loop for method in '(:get :head :post) collect
              `(,method
                ,(compile-routes (applicable-routes method routes)))))))
