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

(defun valid-out (x)
  "Protect against values not suitable for my simple json generator."
  (typecase x
    (null "nil")
    (t x)))

(defjson summary ()
  (format nil "{~{~a~^,~}}"
    (mapcar (lambda (pair)
              (format nil "~s: ~s" (car pair) (valid-out (cdr pair))))
            `(("lisp-implementation-type" . ,(lisp-implementation-type))
              ("lisp-implementation-version" . ,(lisp-implementation-version))
              ("machine-instance" . ,(machine-instance))
              ("machine-type" . ,(machine-type))
              ("machine-version" . ,(machine-version))
              ("software-type" . ,(software-type))
              ("software-version" . ,(software-version))))))

(defjson features ()
  (json-list *features*))

(defjson all-packages ()
  (json-list (list-all-packages)
             :mapcar #'package-name))

(defjson package-symbols (package all)
  (let ((sx) 
        (package (find-package package))) 
    (if (string= all "true")
      (do-symbols (s package) (push s sx))
      (do-external-symbols (s package) (push s sx))) 
    (json-list sx)))

(hunchentoot:define-easy-handler (describe-symbol :uri "/data/describe-symbol")
                                 (package symbol)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((symbol (find-symbol symbol package)))
    (with-output-to-string (stream)
      (describe symbol stream))))

(hunchentoot:define-easy-handler (room-details :uri "/data/room") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (with-output-to-string (*standard-output*)
    (room)))