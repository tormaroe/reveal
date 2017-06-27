(in-package #:data)

(defmacro defjson (resource parameters &body body)
  (let ((path (format nil "/data/~a" (string-downcase (string resource)))))
    `(hunchentoot:define-easy-handler (,resource :uri ,path) ,parameters
       (setf (hunchentoot:content-type*) "application/json")
       ,@body)))

(defun json-list (data &key (sort 'string-lessp) (mapcar 'string))
  (format nil "[~{~s~^,~}]" 
    (sort (mapcar mapcar data) 
          sort)))

;;;

(defjson features ()
  (json-list *features*))

(defjson all-packages ()
  (json-list (list-all-packages)
             :mapcar #'package-name))

(defjson package-symbols (package)
  (let (sx) 
    (do-external-symbols (s (find-package package)) 
      (push s sx)) 
    (json-list sx)))

(hunchentoot:define-easy-handler (describe-symbol :uri "/data/describe-symbol")
                                 (package symbol)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((symbol (find-symbol symbol package)))
    (with-output-to-string (stream)
      (describe symbol stream))))
