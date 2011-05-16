;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

;;;; this code exports json model that is suitable
;;;; to use with examples from http://learningwebgl.com

(defpackage #:json
  (:use #:common-lisp #:abop #:povray)
  (:export #:save-json #:*save-bulk*))

(in-package #:json)

(defvar *vertex-db* nil)
(defvar *positions* nil)
(defvar *normals* nil)
(defvar *indices* nil)
(defvar *uv-map* nil)
(defvar *index* nil)

(defparameter *scale* 0.02)
(defparameter *max-buffer-size* 65000)

(defun push-position (point)
  (push (point-x point) *positions*)
  (push (point-y point) *positions*)
  (push (point-z point) *positions*))

(defun push-normal (point)
  (push (point-x point) *normals*)
  (push (point-y point) *normals*)
  (push (point-z point) *normals*))

(defun push-uv-map (point)
  (unless point (setf point (make-point)))
  (push (point-x point) *uv-map*)
  (push (point-y point) *uv-map*))

(defvar *instance* nil)

(defun rotate (point)
  (or (and (null *instance*) point)
      (rotate-space point (state-orientation *instance*))))

(defun transform (point)
  (or (and (null *instance*) point)
      (let ((rotated (point-scale (rotate point) (state-scale *instance*))))
	(point-add (state-location *instance*) rotated))))

(defun save-vertex (vertex)
  (or (gethash vertex *vertex-db*)
      (progn
	(push-uv-map (vertex-uv-map vertex))
	(push-normal (rotate (vertex-normal vertex)))
	(push-position (transform (vertex-position vertex)))
	(setf (gethash vertex *vertex-db*) (incf *index*)))))

(defun save-elements (name list-of-elements &optional (type "F"))
  (format *stream* (concatenate 'string "~A : [~{~" type "~^,~}],~%")
	  name (nreverse list-of-elements)))

(defun save-to-file ()
  (format *stream* "{~%")
  (save-elements "indices" *indices* "A")
  (save-elements "normals" *normals*)
  (save-elements "pos" *positions*)
  (save-elements "uv" *uv-map*)
  (format *stream* "},~%")
  (setf *vertex-db* (make-hash-table))
  (setf *positions* nil)
  (setf *normals* nil)
  (setf *indices* nil)
  (setf *uv-map* nil)
  (setf *index* -1))

(defun save-face (face)
  (when (> *index* *max-buffer-size*) 
    (save-to-file))
  (dolist (triangle (facetize face))
    (dolist (vertex triangle)
      (push (save-vertex vertex) *indices*))))

(defvar *save-bulk* nil)

(defun save-mesh (mesh)
  (let ((*vertex-db* (make-hash-table)))
    (mapc #'save-face (mesh-faces mesh))
    (unless *save-bulk* (save-to-file))))

(defun save-point (pos)
  (format *stream* "translate : [~F, ~F, ~F],~%"
	  (point-x pos) (point-y pos) (point-z pos)))

(defun save-rotation (orientation)
  (multiple-value-bind (angle1 angle2 angle3)
      (get-transform-angles orientation)
    (format *stream* "angle1 : ~F,~%" angle1)
    (format *stream* "angle2 : ~F,~%" angle2)
    (format *stream* "angle3 : ~F,~%" angle3)))

(defun save-instance (instance)
  (format *stream* "{~%")
  (save-point (point-scale (state-location instance) *scale*))
  (format *stream* "scale : ~F,~%" (* *scale* (state-scale instance)))
  (save-rotation (state-orientation instance))
  (format *stream* "},~%"))

(defun save-object (object)
  (format *stream* "{~%meshes : [~%")
  (map-array (object-mesh-array object) #'save-mesh)
  (format *stream* "],~%")
  (format *stream* "instances : [~%")
  (mapc #'save-instance (object-instances object))
  (format *stream* "]~%},~%"))

(defun merge-instances (instance)
  (let ((*instance* instance))
    (save-mesh (object-instance-mesh instance))))

(defun save-bulk (object)
  (format *stream* "{~%meshes : [~%")
  (mapc #'merge-instances (object-instances object))
  (save-to-file)
  (format *stream* "],~%")
  (format *stream* "instances : [~%")
  (save-instance (make-state))
  (format *stream* "]~%},~%"))

(defun format-json ()
  (format *stream* "[~%")
  (map-library (if *save-bulk* #'save-bulk #'save-object))
  (format *stream* "]~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to save raw model wrap function call like this:
;;
;; (let ((json:*save-bulk* t)) (chestnut))

(defun save-json (&optional file-name)
  (let ((*positions* nil)
	(*normals* nil)
	(*indices* nil)
	(*uv-map* nil)
	(*index* -1))
    (file-safety-net (or file-name "scene.json") #'format-json)))
