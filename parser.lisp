(load "scanner.lisp")

(defvar *index* 0)

(defun inc () (incf *index*))

(defun tree-to-graph (source-file &optional (target-file "graph.dot"))
  (let ((graph (cl-graph:make-graph 'cl-graph:dot-graph
				    :default-edge-type :directed)))
    (make-pairs (parse-file source-file)
	        (lambda (pair)
		  (let ((vertex1 (cl-graph:add-vertex graph
						      (car (car pair))
						      :dot-attributes (list :label (second (car pair)))))
			(vertex2 (cl-graph:add-vertex graph
						      (car (second pair))
						      :dot-attributes (list :label (second (second pair))))))
		    (cl-graph:add-edge-between-vertexes graph
							vertex1
							vertex2
							:dot-attributes '(:label "")))))
    (cl-graph:graph->dot graph target-file)))

(defun make-pairs (lst fn)
  (let ((index *index*))
    (mapc (lambda (elem)
	    (inc)
	    (funcall fn (if (listp elem)
			    (prog1
				(list (list index (car lst)) (list *index* (car elem)))
			      (make-pairs elem fn))
			    (list (list index (car lst)) (list *index* elem)))))
	  (cdr lst))))

(defun parse-file (filename)
  (parser (scanner filename)))

(defvar *lexem-row*)

(defun scan ()
  (pop *lexem-row*))

(defun unscan (lexem)
  (push lexem *lexem-row*) nil)

(defun parser (str)
  (let ((*lexem-row* str))
    (<signal-program>)))

(defun is-correct-lexem (lexem type value)
  (and (eq (car lexem) type)
       (eq (lexem-value (second lexem)) value)))

(defun <signal-program> ()
  (let ((program (<program>)))
    (if (and program (null *lexem-row*))
	(list 'signal-program program)
	(warn "ERROR: Expected end of file."))))

(defun <program> ()
  (if (is-correct-lexem (scan) 'keyword 'PROGRAM)
      (let ((procedure-identifier (<procedure-identifier> (scan))))
	(if procedure-identifier
	    (if (is-correct-lexem (scan) 'delimeter 'semicolon)
		(let ((block_ (<block>)))
		  (if block_
		      (if (is-correct-lexem (scan) 'delimeter 'dot)
			  (list 'program 'PROGRAM procedure-identifier block_ 'dot)
			  (warn "ERROR: dot expected"))
		      (warn "ERROR: <BLOCK> error.")))
		(warn "ERROR: ; expected."))
	    (warn "ERROR: <PROCEDURE-IDENTIFIER> ERROR.")))
      (warn "ERROR: Keyword 'PROGRAM' expected.")))

(defun <block> ()
  (let ((declarations (<declarations>)))
    (if declarations
	(if (is-correct-lexem (scan) 'keyword 'BEGIN)
	    (let ((statements-list (<statements-list>)))
	      (if statements-list
		  (if (is-correct-lexem (scan) 'keyword 'END)
		      (list 'block declarations 'BEGIN statements-list 'END)
		      (warn "ERROR: Expected keyword 'END'"))
		  (warn "ERROR: <STATEMETS-LIST> error.")))
	    (warn "ERROR: Expected keyword 'BEGIN'."))
	(warn "ERROR: <DECLARATIONS> error."))))

(defun <declarations> ()
  (let ((constant-declarations (<constant-declarations>)))
    (if constant-declarations
	(list 'declarations constant-declarations)
	(warn "ERROR: <CONSTANT-DECLARATIONS> error."))))

(defun <constant-declarations> ()
  (let ((ts (scan)))
    (if (is-correct-lexem ts 'keyword 'CONST)
	(let ((constant-declaration-list (<constant-declaration-list>)))
	  (if constant-declaration-list
	      (list 'constant-declarations 'CONST constant-declaration-list)
	      (warn "ERROR: <CONSTANT-DECLARATION-LIST> error.")))
	(prog2
	    (unscan ts)
	    (list 'constant-declarations)))))

(defun <constant-declaration-list> ()
  (labels ((%constant-declaration-list ()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'keyword 'BEGIN)
		   (unscan ts)
		   (let ((constant-declaration (<constant-declaration> ts)))
		     (if constant-declaration
			 (cons constant-declaration (%constant-declaration-list))
			 (warn "ERROR: <CONSTANT-DECLARATION> error.")))))))
    (cons 'constant-declaration-list (%constant-declaration-list))))

(defun <statements-list> ()
  (labels ((%statement-list ()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'keyword 'END)
		   (unscan ts)
		   (let ((statement (<statement> ts)))
		     (if statement
			 (cons statement (%statement-list))
			 (warn "ERROR: <STATEMENT> error.")))))))
    (cons 'statements-list (%statement-list))))

(defun <constant-declaration> (ts)
  (let ((constant-identifier (<constant-identifier> ts)))
    (if constant-identifier
	(if (is-correct-lexem (scan) 'delimeter 'eq-constant)
	    (let ((constant (<constant> (scan))))
	      (if constant
		  (if (is-correct-lexem (scan) 'delimeter 'semicolon)
		      (list 'constant-declaration constant-identifier 'eq-constant constant)
		      (warn "ERROR: ; expected."))
		  (warn "ERROR: <CONSTANT> error.")))
	    (warn "ERROR: ':=' expected"))
	(warn "ERORR: <CONSTANT-IDENTIFIER> erorr."))))

(defun <statement> (ts)
  (let ((variable-identifier (<variable-identifier> ts)))
    (if variable-identifier
	(if (is-correct-lexem (scan) 'delimeter 'eq-variable)
	    (let ((constant (<constant> (scan))))
	      (if constant
		  (if (is-correct-lexem (scan) 'delimeter 'semicolon)
		      (list 'statement variable-identifier 'eq-variable constant)
		      (warn "ERROR: ; expected."))
		  (warn "ERROR: <CONSTANT> erorr.")))
	    (warn "ERROR: ':=' expected"))
	(warn "ERORR: <VARIABLE-IDENTIFIER> erorr."))))

(defun <constant> (ts)
  (let ((unsigned-integer (<unsigned-integer> ts)))
    (if unsigned-integer
	(list 'constant unsigned-integer)
	(warn "ERROR: <UNSIGNED-INTEGER> expected."))))

(defun <constant-identifier> (ts)
  (let ((identifier (<identifier> ts)))
    (if identifier
	(list 'constant-identifier identifier)
	(warn "ERROR: <IDENTIFIER> error."))))

(defun <variable-identifier>(ts)
  (let ((identifier (<identifier> ts)))
    (if identifier
	(list 'variable-identifier identifier)
	(warn "ERROR: <IDENTIFIER> error."))))

(defun <procedure-identifier>(ts)
  (let ((identifier (<identifier> ts)))
    (if identifier
	(list 'procedure-identifier identifier)
	(warn "ERROR: <IDENTIFIER> error."))))

(defun <unsigned-integer>(ts)
  (if (is-correct-lexem ts 'delimeter 'minus)
      (let ((lexem (scan)))
	(if (eq (car lexem) 'unsigned-integer)
	    (let ((unsigned-integer (second lexem)))
	      (list 'unsigned-integer
		    (make-lexem :value (- (lexem-value unsigned-integer))
				:row (lexem-row unsigned-integer)
				:column (1- (lexem-column unsigned-integer)))))
	    (warn "ERROR: <UNSIGNED-INTEGER> expected." )))
      (if (eq (car ts) 'unsigned-integer)
	  ts
	  (warn "ERROR: <UNSIGNED-INTEGER> expected."))))

(defun <identifier>(ts)
  (if (eq (car ts) 'identifier)
	ts
	(warn "ERROR: <IDENTIFIER> expected.")))
