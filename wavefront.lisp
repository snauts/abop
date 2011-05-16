;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defvar *verbose* t)
(defvar *texture* nil)
(defvar *object* nil)
(defvar *store* nil)

(defun create-store ()
  (make-vertex :position (new-array) :uv-map (new-array) :normal (new-array)))

;; --- reading ----------------------------------------------------------------

(defun string-tokenizer (c string)
  (let ((index (position c string)))
    (cons
     (subseq string 0 index)
     (when index (string-tokenizer c (subseq string (1+ index)))))))

(defun plus (a b)
  (and (numberp a) (numberp b) (+ a b)))

(defun parse-vertex (string)
  (mapcar
   (lambda (index table)
     (setf index (plus -1 (parse-integer index :junk-allowed t)))
     (if index (elt table index)))
   (string-tokenizer #\/ string)
   *store*))

(defun empty-string-p (string)
  (= 0 (length string)))

(defun parse-face (string)
  (mapcar
   #'parse-vertex
   (delete-if #'empty-string-p (string-tokenizer #\space string))))

(defun read-face ()
  (add-face *object* (parse-face (read-line *stream*))))

(defun new-object ()
  (let ((name (read-word *stream*)))
    (format *verbose* "object-name: ~A~%" name)
    (setf *object* (get-object name :without-mesh t))
    (setf (object-texture *object*) *texture*)
    (add-empty-mesh *object*)))

(defun read-point (accessor &optional uv-p)
  (vector-push-extend
   (point-read *stream* uv-p)
   (funcall accessor *store*)))

(defun skip-comments ()
  (let ((char (peek-char t *stream* nil nil)))
    (when (eq char #\#)
      (read-line *stream*)
      (skip-comments))
    char))

(defun wavefront-symbol (symbol)
  (intern (symbol-name symbol) (find-package "ABOP")))

(defun read-elements ()
  (when (skip-comments)
    (case (wavefront-symbol (read *stream*))
      (f (read-face))
      (o (new-object))
      (v (read-point #'vertex-position))
      (vn (read-point #'vertex-normal))	
      (vt (read-point #'vertex-uv-map 'ignore-z!))
      (otherwise (read-line *stream*)))
    (read-elements)))

(defun read-wavefront (name &optional texture-name)
  (handler-case
      (with-open-file (*stream* name)
	(let ((*texture* texture-name)
	      (*store* (create-store))
	      (*object* nil))
	  (read-elements)))
    (file-error () (format t "ERROR: could not read ~A~%" name))))

;; --- save stuff back to wavefront -------------------------------------------

(defvar *data-base* nil)
(defvar *uv-base* nil)
(defvar *index* nil)

(export '(*data-base* *index*))

(defparameter *index-fn*
  (make-vertex
   :position (lambda () (incf (vertex-position *index*)))
   :uv-map  (lambda () (incf (vertex-uv-map *index*)))
   :normal (lambda () (incf (vertex-normal *index*)))))

(export 'component-index)
(defun component-index (accessor)
  (funcall (funcall accessor *index-fn*)))

(export 'make-index)
(defun make-index (num)
  (make-vertex :position num :normal num :uv-map num))

(export 'uv-p)
(defun uv-p (accessor)
  (eq accessor #'vertex-uv-map))

(export 'write-component)
(defun write-component (vertex accessor data-base save-fn)
  (let ((point (funcall accessor vertex)))
    (when point
      (setf-if-nil
       (gethash point data-base)
       (funcall save-fn point)))))  

(defun wavefront-point (point prefix accessor)
  (format *stream* "~A ~A~%" prefix (point-write point (uv-p accessor)))
  (component-index accessor))  

(defun wavefront-component (vertex transform data-base prefix accessor)
  (write-component
   vertex
   accessor
   data-base
   (lambda (point)
     (wavefront-point (funcall transform point) prefix accessor))))

(defvar *instance* nil)

(defun rotate (point)
  (rotate-space point (state-orientation *instance*)))

(defun transform (point)
  (point-add
   (state-location *instance*)
   (point-scale (rotate point) (state-scale *instance*))))

(defun save-vertex (vertex face-stream)
  (format
   face-stream
   " ~D/~:[~;~:*~D~]/~:[~;~:*~D~]"
   (wavefront-component vertex #'transform *data-base* "v" #'vertex-position)
   (wavefront-component vertex #'identity *uv-base* "vt" #'vertex-uv-map)
   (wavefront-component vertex #'rotate *data-base* "vn" #'vertex-normal)))

(defun save-face (face)
  (with-output-to-string (face-stream)
    (mapc (lambda (vertex) (save-vertex vertex face-stream)) face)
    (format *stream* "f~A~%" (get-output-stream-string face-stream))))

(defun save-instance (instance)
  (let ((*instance* instance)
	(*data-base* (make-hash-table)))
    (mapc #'save-face (mesh-faces (object-instance-mesh instance)))))

(defun save-object (object)
  (format *stream* "o ~A~%s 1~%" *name*)
  (let* ((*uv-base* (make-hash-table)))
    (mapc #'save-instance (object-instances object))))

(defun wavefront-save (&optional (name "scene.obj"))
  (file-safety-net name (lambda ()
			  (let ((*index* (make-index 0)))
			    (map-library #'save-object)))))
