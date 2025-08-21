(declaim (optimize (debug 3) (safety 3)))

(defmacro define-ecs (name &rest component-classes) 
  (let* ((slots '()))
    (loop
      :for class :in component-classes
      :do (push `(,class :accessor ,class :type ,class) slots)
      )
    `(defclass ,name () ,slots)
    ))

(defclass person ()
  ((name :accessor name :type string)
   (email :accessor email :type string)
   )
  )

(define-ecs db person)

(defun main () 
  (let*
    ((foo (make-instance 'db))
     (_ (print foo))
     )
    )
  )
