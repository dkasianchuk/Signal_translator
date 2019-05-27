(load "parser.lisp")
(defvar *constant-names*)

(defun translate (filename &optional stream)
  (let (*constant-names*)
    (format stream "~A" (traverse (parse-file filename)))))

(defun traverse (tree)
  (case (car tree)
    (<signal-program>
     (traverse (second tree)))
    (<program>
     (format nil ";~A~%~A"
	     (traverse (third tree))
	     (traverse (fourth tree))))
    (<block>
     (format
      nil
      "~A.code~%BEGIN:~%~A~TMOV AX,4c00h~%~TINT 21h~%~Tend BEGIN~%"
      (traverse (second tree))
      (traverse (fourth tree))))
    (<declarations>
     (traverse (second tree)))
    (<constant-declarations>
     (format nil ".const~%~A"
	     (traverse (third tree))))
    (<constant-declaration-list>
     (format nil "~A~A"
	     (traverse (second tree))
	     (traverse (third tree))))
    (<constant-declaration>
     (let ((name (traverse (second tree))))
       (if (find (lexem-value name) *constant-names*)
	   (error
	    "ERROR(row ~A,col ~A).There is a constant '~A' in the program."
	    (lexem-row name)
	    (lexem-column name)
	    name)
	   (prog2
	       (push (lexem-value name) *constant-names*)
	       (format nil "~T~A EQU ~A~%"
		       name
		       (nth-value 1 (traverse (fourth tree))))))))
    (<statements-list>
     (format nil "~A~A"
      (traverse (second tree))
      (traverse (third tree))))
    (<statement>
     (let ((name (traverse (second tree))))
       (if (find (lexem-value name) *constant-names*)
	   (error
	    "ERROR(row ~A, col ~A).'~A' is a constant. You can't change it."
	    (lexem-row name)
	    (lexem-column name)
	    name)
	   (format nil "~A~TMOV ~A, AX~%"
		   (traverse (fourth tree))
		   name))))
    (<constant>
     (if (eq (second tree) 'minus)
	 (let ((value (traverse (third tree))))
	   (values
	    (format nil "~TMOV AX,~A~%~TNEG AX~%" value)
	    (format nil "-~A" value)))
	 (let ((value (traverse (second tree))))
	   (values
	    (format nil "~TMOV AX,~A~%" value)
	    value))))
    ((or <constant-identifier>
	 <variable-identifier>
	 <procedure-identifier>)
     (traverse (second tree)))
    (<identifier> (second tree))
    (<unsigned-integer> (second tree))
    (t "")))
