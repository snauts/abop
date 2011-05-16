;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defconstant +left+ -1)
(defconstant +right+ 1)

(defconstant +open+ -1)
(defconstant +closed+ 1)

(defvar *word* nil)
(defvar *iterations-so-far* 0)
(defvar *total-iterations* 0)

(defun print-word (word)
  (format t "( ")
  (dolist (rule word)
    (let ((name (funcall rule :name))
	  (args (funcall rule :args)))
      (when name 
	(princ name)
	(when args
	  (princ args))
	(princ " "))))
  (format t ")~%")
  word)

(defun traverse (tail left center right)
  (if center
      (traverse 
       (attach-to-queue tail (funcall (funcall center) (cons left right)))
       (cons center left)
       (car right)
       (cdr right))
      (get-queue-head tail)))

(defun rewrite (word)
  (traverse (create-queue) nil (car word) (cdr word)))

(defun draw-system (word)
  (mapc (lambda (letter) (funcall letter :draw)) word))

(defun iteration-count (count)
  (setf *total-iterations* count)
  (clean-up-forms (setf *total-iterations* 0)))
 
(defun do-nothing () nil)

(defun create-rule (&key arg-list draw-fn rule-name expand context)
  (lambda (&optional cmd)
    (case cmd
      (:args arg-list)
      (:name rule-name)
      (:draw (if draw-fn (funcall draw-fn)))
      (otherwise (lambda (ctx) (cond (context (funcall context ctx))
				     (expand (funcall expand))	 
				     (t #'do-nothing)))))))

(defmacro create-common-rule (fn-name command &optional (name nil))
  `(defun ,fn-name (&rest rest) 
     (create-rule
      :arg-list rest
      :rule-name ,name
      :draw-fn (lambda () (apply #',command rest))
      :expand (lambda () (list (apply #',fn-name rest))))))

(create-common-rule [ turtle-push '[)
(create-common-rule ] turtle-pop '])

(create-common-rule { turtle-polygon-start)
(create-common-rule } turtle-polygon-finish)
(create-common-rule dot turtle-polygon-point)
(create-common-rule edge turtle-polygon-forward)

(create-common-rule roll turtle-roll)
(create-common-rule turn  turtle-turn)
(create-common-rule pitch turtle-pitch)
(create-common-rule color turtle-set-color)

(create-common-rule roll! turtle-roll 'roll)
(create-common-rule turn! turtle-turn 'turn)
(create-common-rule pitch! turtle-pitch 'pitch)

(create-common-rule flip turtle-flip-up)
(create-common-rule jump turtle-jump)
(create-common-rule forward turtle-forward)
(create-common-rule radius turtle-set-radius)
(create-common-rule surface turtle-set-surface)
(create-common-rule obj turtle-object)
(create-common-rule move turtle-teleport)

(defun get-next-rule (word fn)
  (if word (funcall fn (funcall (car word) :name) (cdr word))))

(defun nesting (x)
  (cond ((eq x '[) +1)
	((eq x ']) -1)
	(t 0)))

(with-compilation-unit nil
  (defun skip-branch (word count)
    (if (= 0 count) word (skip-branch-more word count)))
  (defun skip-branch-more (word count)	
    (get-next-rule word (lambda (x r) (skip-branch r (+ count (nesting x)))))))

(defun is-parenthesis? (symbol side state)
  (eq (if (= side state) '] '[) symbol))

(defun get-args (word list)
  (if (consp (first list))
      (progn
	(setf (first list) (funcall word :args))
	(rest list))
      list))

(with-compilation-unit nil
  (defun match-side (word match side)
    (or (or (null match) (equal match '(*)))
	(match-symbol word (car match) match side)))
  (defun match-symbol (word symbol match side)
    (get-next-rule 
     word
     (lambda (rule-name rest)
       (cond ((eq rule-name symbol)
	      (setf (car match) (car word))
	      (match-side rest (get-args (car word) (cdr match)) side))
	     ((not rule-name)
	      (match-side rest match side))	   
	     ((eq symbol '*)
	      (match-side (skip-branch word side) (cddr match) side))
	     ((is-parenthesis? rule-name side +open+)
	      (match-side (skip-branch rest side) match side))
	     ((is-parenthesis? rule-name side +closed+) 
	      (if (= side +left+) (match-side rest match side) nil))
	     (t nil))))))

(defun collect-args (match-list)
  (when match-list
    (if (consp (first match-list))
	(append (first match-list) (collect-args (rest match-list)))
	(collect-args (rest match-list)))))

(defun match (context l-match r-match)
  (if (and (match-side (car context) l-match +left+)
	   (match-side (cdr context) r-match +right+))
      (or (collect-args (nconc l-match r-match)) t)))

(defun find-rip (word)
  (get-next-rule 
   word 
   (lambda (name rest) (if (eq name :rip) word (find-rip rest)))))

(with-compilation-unit nil
  (defun ripper (word)
    (let ((rip-point (find-rip word)))
      (when rip-point (rip-to-bracket rip-point))
      word))
  (defun rip-to-bracket (word)
    (if word (stitch word (skip-branch word 1))))
  (defun stitch (start end)
    (if end (setf (car start) (])))
    (setf (cdr start) end)
    (ripper end)))
  
(defun rip ()
  (create-rule
   :rule-name :rip
   :expand (lambda () nil)))

(defvar *word-debug* #'identity)

(defun produce (word)
  (dotimes (*iterations-so-far* *total-iterations* word)
    (setf word (funcall *word-debug* (ripper (rewrite word))))))

(defun filter-rules (word set)
  (flet ((filter (rest-word)
	   (let ((name (first rest-word))
		 (args (second rest-word)))
	     (cond ((or (consp name) (not (member name set))) nil)
		   ((atom args) (list name))
		   (t (list name args))))))
    (mapcon #'filter word)))

