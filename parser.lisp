(load "scanner.lisp")

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
	(list '<signal-program> program)
	(warn "ERROR: Expected end of file."))))

(defun <program> ()
  (let ((start (scan)))
    (if (is-correct-lexem start 'keyword 'PROGRAM)
	(let ((procedure-identifier (<procedure-identifier> (scan))))
	  (when procedure-identifier
	    (let ((ts (scan)))
	      (if (is-correct-lexem ts 'delimeter 'semicolon)
		  (let ((block_ (<block>)))
		    (when block_
		      (if (is-correct-lexem (scan) 'delimeter 'dot)
			  (list '<program> 'PROGRAM procedure-identifier block_)
			  (warn "ERROR: dot expected at the end"))))
		  (warn "ERROR: ; expected at line: ~S, column ~S."
			(lexem-row (second ts))
			(lexem-column (second ts)))))))
	(warn "ERROR: Keyword 'PROGRAM' expected at line: ~S, column ~S."
	      (lexem-row (second start))
	      (lexem-column (second start))))))

(defun <block> ()
  (let ((declarations (<declarations>)))
    (when declarations
      (let ((begin (scan)))
	(if (is-correct-lexem begin 'keyword 'BEGIN)
	    (let ((statements-list (<statements-list>)))
	      (when statements-list
		(let ((end (scan)))
		  (if (is-correct-lexem end 'keyword 'END)
		      (list '<block> declarations 'BEGIN statements-list 'END)
		      (warn "ERROR: Expected keyword 'END' at line: ~S, column ~S."
			    (lexem-row (second end))
			    (lexem-column (second end)))))))
	    (warn "ERROR: Expected keyword 'BEGIN' at line: ~S, column ~S."
		  (lexem-row (second begin))
		  (lexem-column (second begin))))))))

(defun <declarations> ()
  (let ((constant-declarations (<constant-declarations>)))
    (when constant-declarations
      (list '<declarations> constant-declarations))))

(defun <constant-declarations> ()
  (let ((ts (scan)))
    (if (is-correct-lexem ts 'keyword 'CONST)
	(let ((constant-declaration-list (<constant-declaration-list>)))
	  (when constant-declaration-list
	    (list '<constant-declarations> 'CONST constant-declaration-list)))
	(prog2
	    (unscan ts)
	    (list '<constant-declarations>)))))

(defun <constant-declaration-list> ()
  (labels ((%constant-declaration-list ()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'keyword 'BEGIN)
		   (prog2
		       (unscan ts)
		       (list '<constant-declaration-list>))
		   (let ((constant-declaration (<constant-declaration> ts)))
		     (when constant-declaration
		       (cons '<constant-declaration-list>
			     (list constant-declaration (%constant-declaration-list)))))))))
    (%constant-declaration-list)))

(defun <statements-list> ()
  (labels ((%statement-list ()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'keyword 'END)
		   (prog2
		       (unscan ts)
		       (list '<statements-list>))
		   (let ((statement (<statement> ts)))
		     (when statement
		       (cons '<statements-list>
			     (list statement (%statement-list)))))))))
    (%statement-list)))

(defun <constant-declaration> (ts)
  (let ((constant-identifier (<constant-identifier> ts)))
    (when constant-identifier
      (let ((eq-constant (scan)))
	(if (is-correct-lexem eq-constant 'delimeter 'eq-constant)
	    (let ((constant (<constant> (scan))))
	      (when constant
		(let ((semi (scan)))
		  (if (is-correct-lexem semi 'delimeter 'semicolon)
		      (list '<constant-declaration> constant-identifier 'eq-constant constant)
		      (warn "ERROR: ; expected at line ~S, column ~S."
			    (lexem-row (second semi))
			    (lexem-column (second semi)))))))
	    (warn "ERROR: ':=' expected at line ~S, column ~S."
		  (lexem-row (second eq-constant))
		  (lexem-column (second eq-constant))))))))

(defun <statement> (ts)
  (let ((variable-identifier (<variable-identifier> ts)))
    (when variable-identifier
      (let ((eq-variable (scan)))
	(if (is-correct-lexem eq-variable 'delimeter 'eq-variable)
	    (let ((constant (<constant> (scan))))
	      (when constant
		(let ((semi (scan)))
		  (if (is-correct-lexem semi 'delimeter 'semicolon)
		      (list '<statement> variable-identifier 'eq-variable constant)
		      (warn "ERROR: ; expected at line ~S,column ~S."
			    (lexem-row (second semi))
			    (lexem-column (second semi)))))))
	    (warn "ERROR: ':=' expected at line ~S,column ~S."
		  (lexem-row (second eq-variable))
		  (lexem-column (second eq-variable))))))))

(defun <constant> (ts)
  (if (is-correct-lexem ts 'delimeter 'minus)
      (let ((unsigned-integer (<unsigned-integer> (scan))))
	(when unsigned-integer
	  (list '<constant> 'minus unsigned-integer)))
      (let ((unsigned-integer (<unsigned-integer> ts)))
	(when unsigned-integer
	  (list '<constant> unsigned-integer)))))

(defun <constant-identifier> (ts)
  (let ((identifier (<identifier> ts)))
    (when identifier
      (list '<constant-identifier> identifier))))

(defun <variable-identifier>(ts)
  (let ((identifier (<identifier> ts)))
    (when identifier
      (list '<variable-identifier> identifier))))

(defun <procedure-identifier>(ts)
  (let ((identifier (<identifier> ts)))
    (when identifier
      (list '<procedure-identifier> identifier))))

(defun <unsigned-integer>(ts)
  (if (eq (car ts) 'unsigned-integer)
      (list '<unsigned-integer> (second ts))
      (warn "ERROR: <UNSIGNED-INTEGER> expected at line ~S, column ~S."
	    (lexem-row (second ts))
	    (lexem-column (second ts)))))

(defun <identifier>(ts)
  (if (eq (car ts) 'identifier)
      (list '<identifier> (second ts))
      (warn "ERROR: <IDENTIFIER> expected at line ~S,column ~S."
	    (lexem-row (second ts))
	    (lexem-column (second ts)))))
