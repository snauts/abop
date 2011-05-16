;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defvar *options* nil)
(defvar *angle* nil)
(defvar *length* nil)
(defvar *macros* nil)
(defvar *axiom* nil)
(defvar *rules* nil)
(defvar *imports* nil)
(defvar *return* nil)

(defparameter *name-replace*
  '((- . -t)
    (+ . +t)))

(defmacro command-alias (new-name old-name &rest default-args)
  `(defun ,new-name (&rest args)
     (apply #',old-name (or args (list ,@default-args)))))

(command-alias f forward *length*)
(command-alias +t turn *angle*)

(defun -t (&optional angle)
  (let ((default-angle *angle*))
    (create-rule
     :draw-fn (lambda () (turtle-turn (- (or angle default-angle))))
     :expand (lambda () (list (if angle (-t angle) (-t)))))))

(defun get-option (option-name)
  (rest (assoc option-name *options*)))

(defun check-symbol (x)
  (if (symbolp x) (symbol-name x)))

(defun symbol-position (symbol rule)
  (position symbol rule :key #'check-symbol :test #'equal))

(defun search-symbol (symbol rule)
  (let ((offset (symbol-position symbol rule)))
    (if offset (subseq rule (1+ offset)))))

(defun simple-rule-p (rule)
  (some #'atom rule))

(defun context-rule-p (rule)
  (some (lambda (x) (or (symbol-position "<" x) (symbol-position ">" x)))
	(if (simple-rule-p rule) (list rule) rule)))

(defun get-simple-rule-name (simple-rule)
  (or (search-symbol "<" simple-rule) simple-rule))

(defun get-rule-name (rule)
  (get-simple-rule-name (if (simple-rule-p rule) rule (first rule))))

(defun get-rule-args (rule-name)
  (if (consp (second rule-name)) (second rule-name)))

(defun format-one-word (rule-list count)
  (make-list
   count
   :initial-element (cons (first rule-list) (get-rule-args rule-list))))

(defun do-word-format (rule-list)
  "converts (A(5) 3 B(6) C) -> ((A 5) (B 6) (B 6) (B 6) (C))"
  (let ((count 1))
    (flet ((update () (prog1 count (setf count 1))))
      (mapcon 
       (lambda (x)
	 (cond ((numberp (first x)) (setf count (first x)) nil)
	       ((atom (first x)) (format-one-word x (update)))
	       (t nil)))
       rule-list))))

(defun sublis-list (a-list word)
  "same sublis, but in list, not in tree"
  (mapcar (lambda (x) (or (cdr (assoc x a-list)) x)) word))

(defun format-word (word)
  (do-word-format (sublis-list *name-replace* word)))

(defun get-replacement (rule)
  (search-symbol "->" rule))

(defun get-condition (simple-rule)
  (let ((condition (search-symbol "?" simple-rule)))
    (if condition (first condition))))

(defun l-side-context (simple-rule)
  (let ((offset (symbol-position "<" simple-rule)))
    (if offset (subseq simple-rule 0 offset))))

(defun r-side-context (simple-rule)
  (let ((offset1 (symbol-position ">" simple-rule))
	(offset2 (or (symbol-position "?" simple-rule)
		     (symbol-position "->" simple-rule))))
    (when (and offset1 offset2)
      (subseq simple-rule (1+ offset1) offset2))))

(defun quote-list (list)
  `(list ,@(mapcar (lambda (x) `',x) list)))

(defun get-context (l-side r-side)
  (when (or l-side r-side)
    `(match context ,(quote-list l-side) ,(quote-list r-side))))

(defun add-test-clause (exp)
  (when exp
    `((setf test ,exp)
      (if (not test) (return-from rule nil)))))

(defun initialize-args (args)
  (let ((i -1))
    (mapcar (lambda (x) (list 'setf x `(elt test ,(incf i)))) args)))

(defun format-simple-rule (simple-rule)
  (let* ((l-side (l-side-context simple-rule))
	 (r-side (r-side-context simple-rule))
	 (context (get-context l-side r-side))
	 (condition (get-condition simple-rule))
	 (args (collect-args (append l-side r-side))))
    `((block rule
	(let ,args
	  (setf test nil) ;; just to suppress warning
	  ,@(add-test-clause context)
	  ,@(if context (initialize-args args))
	  ,@(add-test-clause condition)
	  (setf result (list ,@(format-word (get-replacement simple-rule))))
	  t))
      result)))

(defun format-rule (rule)
  `(let (test result)
     (cond ,@(if (simple-rule-p rule)
		 (list (format-simple-rule rule))
		 (mapcar #'format-simple-rule rule)))))

(defun format-production (rule)
  (if (not (context-rule-p rule))
      `(:expand (lambda () ,(format-rule rule)))
      `(:context (lambda (context) ,(format-rule rule)))))

(defun get-draw-function (name)
  (let ((draw-function-body (get-option name)))
    (if draw-function-body `(:draw-fn (lambda () ,draw-function-body)))))

(defvar *rule-table* nil)

(defun compile-rule (rule nameless)
  (let ((rule-name (get-rule-name rule)))
    (push `(cons ',(first rule-name) #',(first rule-name)) *rule-table*)
    `(,(first rule-name) ,(get-rule-args rule-name)
       (create-rule
	:arg-list (list ,@(get-rule-args rule-name))
	:rule-name ',(unless nameless (first rule-name))
	,@(get-draw-function (first rule-name))
	,@(format-production rule)))))

(defun check-compile-rule (rule)
  (if (eq :nameless (first rule))
      (compile-rule (rest rule) t)
      (compile-rule rule nil)))

(defun compile-rules (rule)
  (when rule
    (cons (check-compile-rule (first rule))
	  (compile-rules (rest rule)))))

(defun expand-macro (macro place)
  (let ((replacement (copy-tree (get-replacement macro)))
	(arguments (second macro)))
    (if (not (consp arguments))
	(nconc replacement (cdr place))
	(nconc (sublis (pairlis arguments (second place)) replacement)
	       (cddr place)))))

(defun replace-in-place (macro place)
  (setf (cdr place) (cdr macro)
	(car place) (car macro)))

(defun replace-macro (macro place)
  (when macro 
    (replace-in-place (expand-macro macro place) place)))
  
(defun walk-tree (tree fn)
  (when tree
    (if (atom (first tree))
	(funcall fn tree)
	(walk-tree (first tree) fn))
    (walk-tree (rest tree) fn)))

(defun choose-macro (macros place)
  (replace-macro (assoc (first place) macros) place))

(defun apply-macros (macros rules)
  (walk-tree rules (lambda (x) (choose-macro macros x)))
  rules)

(defun apply-macro-in-macros (macros)
  (mapc (lambda (x) (apply-macros (remove x macros) x)) macros))

(defun wrap-imports (word &optional rules)
  (let ((ret `(list ,@word)))
    `(let ,*imports*
       (declare #+sbcl(sb-ext:muffle-conditions style-warning))
       ,(if (null rules) ret `(labels (,@rules) ,ret))))) ; speeds up elm 3x

(defun l-system-body ()
  (let* ((*rule-table* nil)
	 (compiled-rules (compile-rules (apply-macros *macros* *rules*))))
    (wrap-imports *rule-table* compiled-rules)))

(defun build-axiom (rules)
  (let ((axiom (format-word (apply-macros *macros* *axiom*))))
    (labels ((expand (x) (eval (wrap-imports (rest x))))
	     (lookup (x) (or (cdr (assoc x rules)) (fdefinition x))))	     
      (mapcar (lambda (x) (apply (lookup (first x)) (expand x)))  axiom))))

(defun l-execute ()
  (apply-macro-in-macros *macros*)
  (produce (build-axiom (eval (l-system-body)))))

(defmacro add-rule (rule)
  `(push ',rule *rules*))

(defmacro add-macro (macro)
  `(push ',macro *macros*))

(defmacro add-option (name body)
  `(push (cons ',name ',body) *options*))

(defun add-variables (names &rest values)
  (loop 
     for n in names
     for v in values
     do (push (list n v) *imports*)))

(defmacro import-vars (&rest args)
  `(add-variables ',args ,@args))

(defmacro set-axiom (axiom)
  `(setf *axiom* ',axiom))

(defun use-axiom (axiom)
  (setf *axiom* axiom))

(defun set-angle (angle)
  (setf *angle* angle))

(defun set-length (length)
  (setf *length* length))

(defmacro produce-l-system (&body body)
  `(let ((*imports* nil)
	 (*options* nil)
	 (*macros* nil)
	 (*axiom* nil)
	 (*rules* nil)
	 (*angle* 5.0)
	 (*length* 10.0))
     ,@body
     (l-execute)))
    
(defun listify-rule (rule)
  (let ((name (funcall rule :name))
	(args (funcall rule :args)))
    (cond ((and name args) (list name args))
	  ((null args) (list name))
	  (t nil))))

(defun convert-readable (word)
  (mapcan #'listify-rule word))

(defmacro with-output-to-list (&body body)
  `(convert-readable (produce-l-system ,@body)))

(defmacro with-debug (&body body)
  `(let ((*word-debug* #'print-word))
     (produce-l-system ,@body)))

(defmacro draw-l-system (body)
  `(progn
     (draw-system (produce-l-system ,@body))
     (draw-all-branching)))

(defmacro with-layers (&body body)
  `(with-system
     (with-library
       (with-segments
	 (with-turtle
	   ,@body)))))

(defmacro with-graphics (&body body)
  `(with-layers
     (draw-l-system ,body)
     (run-opengl)))

(defvar *output-file* nil)

(defun set-output-file (name)
  (setf *output-file* name))

(defmacro with-output-to-file (&body body)  
  `(let ((*disable-opengl* (lambda () (wavefront-save *output-file*)))
	 (*output-file* "scene.obj"))
     (with-graphics ,@body)))

(defmacro with-output-to-stdio (&body body)
  `(let ((*disable-opengl* #'rib:print-rib)
	 (*verbose* nil))
     ,@body))
 
