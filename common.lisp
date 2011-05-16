;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(defpackage #:abop
  (:use #:common-lisp))

(in-package #:abop)

(declaim (inline sfloat float-equal))

(defun sfloat (value) (coerce value 'single-float))

(defun float-equal (f1 f2)
  (> single-float-epsilon (abs (- f1 f2))))

(defun to-radians (angle)
  (sfloat (* angle (/ pi 180.0))))

(export 'to-degrees)
(defun to-degrees (angle)
  (sfloat (* (/ angle pi) 180.0)))

(defun unit-random ()
  (/ (random most-positive-fixnum) 
     (sfloat (1- most-positive-fixnum))))

(defun file-probe (name)
  (open name :direction :probe))

(defun indexed-name (prefix extension &optional (number 0) (fn #'file-probe))
  (let ((name (format nil "~A-~5,'0d~A" prefix number extension)))
    (if (funcall fn name)
	(indexed-name prefix extension (incf number) fn)
	name)))

(export 'setf-if-nil)
(defmacro setf-if-nil (x y)
  `(or ,x (setf ,x ,y)))

(defun is-whitespace (char)
  (member char '(#\space #\tab #\newline)))

(defun read-word (stream)
  (peek-char t stream)
  (with-output-to-string (out)
    (loop for c = (peek-char nil stream) do
         (unless (and c (not (is-whitespace c)))
	   (loop-finish))
         (write-char c out)
         (read-char stream))))

(export 'map-array)
(defun map-array (array fn)
  (dotimes (i (length array))
    (funcall fn (aref array i))))

(defun length=1 (x)
  (and (consp x) (null (cdr x))))

(defun sym (&rest args)
  (intern (format nil "~{~a~}" args)))

(export '*stream*)
(defvar *stream* nil)

(export 'file-safety-net)
(defun file-safety-net (name fn)
  (handler-case
      (with-open-file (*stream* name :direction :output :if-exists :supersede)
	(funcall fn))
    (file-error () (format t "ERROR: could not write ~A~%" name))
    (:no-error (ret) (progn (format t "done saving ~A~%" name) ret))))

(defun first-or-self (something)
  (if (consp something)
      (first something)
      something))

(export 'new-symbol)
(defun new-symbol (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun slot-symbols (name slots)
  (mapcar (lambda (slot) (new-symbol name "-" (first-or-self slot))) slots))

(defmacro def-exported-struct (name-and-options &rest slots)
  (let ((name (first-or-self name-and-options)))
    `(progn
       (defstruct ,name-and-options ,@slots)
       (export '(,(new-symbol "MAKE-" name)
		 ,@(slot-symbols name slots))))))

(export 'concstrs)
(defun concstrs (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun dbg (x)
  (format t "~A~%" x)
  x)

(cffi:defcfun ("noise3D" noise3D) :float
  (x :float)
  (y :float)
  (z :float))

(defun noise (p)
  (noise3D (point-x p) (point-y p) (point-z p)))

;; --- queue ------------------------------------------------------------------

(defstruct queue length start tail)

(defun create-queue (&optional some-list)
  (let ((head-list (cons :head some-list)))
    (make-queue :length (length some-list)
		:tail (last head-list)
		:start head-list)))

(defun get-queue-head (queue)
  (cdr (queue-start queue)))

(defun attach-to-queue (queue some-list)
  (when some-list
    (nconc (queue-tail queue) some-list)
    (setf (queue-tail queue) (last some-list))
    (incf (queue-length queue) (length some-list)))
  queue)

;; --- generic wrapper --------------------------------------------------------

(defvar *clean-up* nil)

(defmacro clean-up-forms (&body commands)
  `(push (lambda () ,@commands) *clean-up*))

(defmacro with-system (&body body)
  `(let ((*clean-up* nil))
     (unwind-protect
	  (progn ,@body (values))
       (mapc #'funcall *clean-up*))))
