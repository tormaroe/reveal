(in-package #:data)

(defmacro defjson (resource parameters &body body)
  (let ((path (format nil "/data/~a" (string-downcase (string resource)))))
    `(hunchentoot:define-easy-handler (,resource :uri ,path) ,parameters
       (setf (hunchentoot:content-type*) "application/json")
       ,@body)))


(defjson features ()
  (format nil "[~{~s~^,~}]"
          (sort (mapcar #'string *features*)
                #'string-lessp)))

(defjson all-packages ()
  (format nil "[~{~s~^,~}]"
          (sort (mapcar #'package-name (list-all-packages))
                #'string-lessp)))

(defjson package-symbols (package)
  (let (sx) 
    (do-external-symbols (s (find-package package)) 
      (push s sx)) 
    (format nil "[~{~s~^,~}]"
            (sort (mapcar #'string sx) 
                  #'string-lessp))))