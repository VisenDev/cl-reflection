(declaim (optimize (debug 3) (safety 3)))

(defun to-string (arg) 
  (format nil "~a" arg)
  )

(defun string-concat (strs) 
  (if (= 1 (length strs))
    (car strs)
    (concatenate 'string (car strs) (string-concat (rest strs)))
    )
  )

(defun symbol-concat (&rest args)
  (let*
    ((strs (mapcar #'to-string args))
     (capstrs (mapcar #'string-upcase strs))
     (str (string-concat capstrs))
     )
    (intern str)
    ))

(defun tostring-concat (&rest args)
  (string-downcase (format nil "~{~a~}" args))
  )

;(defun generate-struct-pretty-print-fn (name fields)
;  (lambda (struct)
;    (concatenate
;     'string
;     (format nil "(make-~a" name)
;     (string-concat
;      (loop :for field in fields  
;            :collect (tostring-concat ":" field  





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

(defstruct foo
  bar bip bapp)

(defun generate-serialize-format-string (fields)
  (print fields)
  (format nil "{~{~a~}}"
          (loop
            :with prepend-comma = nil
            :for field in fields
            :for str = (string-downcase (symbol-name (print field)))
            :collect (if prepend-comma
                       (format nil ", ~s: ~~a" str)
                       (format nil "~s: ~~a" str))
            :do (setq prepend-comma t)
          )
  ))

(defun generate-serialize-accessor-forms (name fields)
  (loop
    :for field in fields
    :collect (list (symbol-concat name '- field) 'obj)
    )
  )

(defmacro def-serializer (name fields)
  `(defun ,(symbol-concat 'serialize- name) (obj)
     (format
      nil
      ,(generate-serialize-format-string fields)
      ,@(generate-serialize-accessor-forms name fields)
      )))

(defun make-serializer (name fields)
  (eval
   `(defun ,(symbol-concat 'serialize- name) (obj)
      (format
       nil
       ,(generate-serialize-format-string fields)
       ,@(generate-serialize-accessor-forms name fields)
       ))
   )))






      
      

(defun serialize-foo (foo)
  (format
   nil 
   "(make-foo :bar ~a :bip ~a :bapp ~a)"
   (foo-bar foo) (foo-bip foo) (foo-bapp foo)
   )
  )


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
