(defmacro reflective-struct (name fields)
  (let* ((metadata-name (intern (string-upcase (concatenate 'string "+" (symbol-name name) "-metadata+"))))
         )
    `(progn
       (defconstant
         ,metadata-name
         (quote
           ,(mapcar
              #'(lambda (field)
                  (list
                    field
                    (intern (string-upcase (concatenate 'string (symbol-name name) "-" (symbol-name field)))))
                  )
              fields)))
       (defstruct ,name ,@fields)
       )
    )
  )

(reflective-struct
  foo
  (bar bip bapp))

(defun serialize-reflective-struct (metadata value) 
  (format t "(make-foo")
  (loop :for field in metadata
        :do (format t " :~a ~s" (string-downcase (symbol-name (first field))) (funcall (second field) value))
        )
  (format t ")")
  )


;(print +foo-metadata+)
(defparameter *value* (make-foo))
(serialize-reflective-struct +foo-metadata+ *value*)
