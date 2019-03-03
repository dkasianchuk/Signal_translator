(defstruct lexem
  value
  row
  column)

(defmethod print-object ((lex lexem) stream)
  (format stream "~S" (lexem-value lex)))

(defun create-lexem (value row column)
  (make-lexem :value value
	      :row row
	      :column column))

(defun is-keyword (word)
  (find word '(PROGRAM BEGIN END CONST)))

(defun is-delimeter (dm)
  (cond
    ((eq dm #\;) 'semicolon)
    ((eq dm #\.) 'dot)
    ((eq dm #\=) 'eq-constant)
    ((eq dm #\-) 'negative)))

(defun is-whitespace (symb)
  (and symb (find symb '(#\Return #\Space #\Tab #\Newline #\Vt))))

(defun scanner (&optional (filename "test.txt"))
  (with-open-file (stream filename)
    (do* ((read-value (list (read-char stream nil) nil 1 1)
		      (analyse stream
			       (first read-value)
			       (third read-value)
			       (fourth read-value)))
	  (rezult nil
		  (cons (second read-value) rezult)))
	 ((not (car read-value))
	  (reverse (remove nil rezult))))))

(defmacro change-row-col (row col &optional (delta 1))
  `(if (or (eq next-symb #\Newline)
	   (eq next-symb #\Return))
       (list (+ ,row 1) 0)
       (list ,row (+ ,col ,delta))))

(defun analyse (stream input row column)
  (cond
    ((is-whitespace input)
     (do* ((next-symb input
		      (read-char stream nil))
	   (row-col (change-row-col row column 0)
		    (change-row-col (car row-col) (second row-col))))
	  ((not (is-whitespace next-symb))
	   (list next-symb nil (car row-col) (second row-col)))))
    ((is-delimeter input)
     (list (read-char stream nil)
	   (list 'delimeter
		 (create-lexem (is-delimeter input)
			       row
			       column))
	   row
	   (1+ column)))
    ((digit-char-p input)
     (do* ((next-symb (read-char stream nil) (read-char stream nil))
	   (buff (list next-symb input) (cons next-symb buff)))
	  ((not (digit-char-p next-symb))
	   (list next-symb
		 (list 'unsigned-integer
		       (create-lexem (reduce (lambda(acc elem)
					       (+ (* acc 10)(digit-char-p elem)))
					     (reverse (cdr buff))
					     :initial-value 0)
				     row
				     column))
		 row
		 (+ column (1- (length buff)))))))
    ((alpha-char-p input)
     (do* ((next-symb (read-char stream nil) (read-char stream nil))
	   (buff (list next-symb input) (cons next-symb buff)))
	  ((not (alphanumericp next-symb))
	   (list next-symb
		 (let ((value (intern (coerce
				       (mapcar #'char-upcase (reverse (cdr buff)))
				       'string))))
		   (list (if (is-keyword value)
			     'keyword
			     'identifier)
			 (create-lexem value row column)))
		 row
		 (+ column (1- (length buff)))))))
    ((eq input #\:)
     (let ((symbol (read-char stream nil)))
       (if (eq symbol #\=)
	   (list (read-char stream nil)
		 (list 'delimeter
		       (create-lexem 'eq-variable
				     row
				     column))
		 row
		 (+ column 2))
	   (format t "ERROR(line ~S,column ~S): Expected '=' after ':', but '~A' found" row column (string symbol)))))
    ((eq input #\( )
     (if (eq (read-char stream nil) #\*)
	 (do* ((next-symb (read-char stream nil) (read-char stream nil))
	       (row-col (change-row-col row column 3)
			(change-row-col (car row-col) (second row-col))))
	      ((if next-symb
		   (and (eq next-symb #\*)
			(do ((last-char (read-char stream nil)(read-char stream nil))
			     (count 0 (+ count 1)))
			    ((not (eq last-char #\*))
			     (cond
			       ((or (eq last-char #\Newline)
				    (eq last-char #\Return))
				(and (incf (car row-col))
				     (setf (second row-col) 0)
				     nil))
			       ((eq last-char #\))
				(setf (second row-col) (+ (+ (second row-col) count) 2))
				t)
			       ((null last-char)
				(format t "ERROR(line ~S,column ~S): Error end of file." (car row-col)
				       (second row-col)))
			       (t (and (setf (second row-col) (1+ (+ (second row-col) count)))
				       nil))))))
		   (format t "ERROR(line ~S,column ~S): Error end of file." (car row-col)
			   (second row-col)))
	       (list (read-char stream nil)
		     nil
		     (car row-col)
		     (second row-col))))
	 (format t "ERROR(line ~S,column ~S): '*' expected after '('." row column)))
    ((null input) nil)
    (t (format t "ERROR(line ~S,column ~S): Illegal symbol '~A'" row column (string input)))))

