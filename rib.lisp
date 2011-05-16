;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(defpackage #:rib
  (:use #:common-lisp #:abop #:povray)
  (:export #:save-rib #:print-rib))

(in-package #:rib)

(defun rib-point (p)
  (format *stream* " ~,6F ~,6F ~,6F" (point-x p) (point-y p) (point-z p)))

(defun output (&rest args)
  (apply #'format (cons *stream* args)))

(defmacro with-square-braces (&body body)
  `(progn (output "[") ,@body (output " ]~%")))

(defvar *db* nil)
(defvar *faces* nil)

(defun save-face-polygon-size ()
  (with-square-braces
    (dolist (i *faces*)
      (output " ~A" (length i)))))

(defun iterate-faces (fn)
  (dolist (face *faces*)
    (dolist (vertice face)
      (funcall fn vertice))))
	 
(defmacro with-face-vertice (&body body)
  `(with-square-braces
     (iterate-faces
      (lambda (vertice)
	,@body))))

(defun save-face-point-indices ()
  (let ((number -1))
    (with-face-vertice
      (let ((pos (vertex-position vertice)))
	(output " ~A" (setf-if-nil (gethash pos *db*) (incf number)))))))

(defun save-face-points ()
  (let ((number 0))
    (output "\"P\" ")
    (with-face-vertice
      (let* ((pos (vertex-position vertice))
	     (index (gethash pos *db*)))
	(when (and index (= index number))
	  (rib-point pos)
	  (incf number))))))

(defun save-face-normals ()
  (output "\"facevarying normal N\" ")
  (with-face-vertice
    (let ((normal (vertex-normal vertice)))
      (rib-point normal))))

(defun save-face-uv-map ()
  (output "\"facevarying float[2] st\" ")
  (with-face-vertice
    (let ((uv-map (vertex-uv-map vertice)))
      (if (null uv-map)
	  (output " 0.0 0.0")
	  (output " ~A ~A" (point-x uv-map) (- 1.0 (point-y uv-map)))))))

(defun find-last-dot (str)
  (let ((last (length str)))
    (dotimes (i (length str) last)
      (when (eq #\. (char str i))
        (setf last i)))))

(defun format-rib-texture-name (texture)
  (if (null texture)
      (output "Surface \"matte\"~%")
      (let ((bare (subseq texture 0 (find-last-dot texture))))
	(output "Surface \"texmap\" ")
	(output "\"string texname\" [\"./~A\"] "
		(concatenate 'string bare ".tex"))
	(output "\"string texmask\" [\"./~A\"]~%"
		(concatenate 'string bare "-mask.tex"))
	(output "# texture name ~A~%" bare))))
    
(defun rib-mesh (mesh)
  (let ((*faces* (mesh-faces mesh))
	(*db* (make-hash-table)))
    (when *faces*
      (setf (mesh-name mesh) *number*)
      (output "ObjectBegin ~A~%" *number*)
      (output "Declare \"st\" \"facevarying float[2]\"~%")
      (output "Declare \"N\" \"facevarying normal\"~%")
      (output "PointsPolygons~%")
      (save-face-polygon-size)
      (save-face-point-indices)
      (save-face-points)
      (save-face-normals)
      (save-face-uv-map)
      (output "ObjectEnd~%")
      (incf *number*))))

(defun rib-translate (point)
  (output "Translate ")
  (rib-point point)
  (output "~%"))

(defun rib-scale (scale)
  (output "Scale ~A ~A ~A~%" scale scale scale))

(defun rib-rotate (orientation)
  (multiple-value-bind (angle1 angle2 angle3)
      (get-transform-angles orientation)
    (output "Rotate ~A 0.0 1.0 0.0~%" (to-degrees angle1))
    (output "Rotate ~A 1.0 0.0 0.0~%" (to-degrees angle2))
    (output "Rotate ~A 0.0 1.0 0.0~%" (to-degrees angle3))))

(defun rib-color (color)
  (output "Color [~A ~A ~A]~%" 
	  (color-r color)
	  (color-g color)
	  (color-b color)))

(defun rib-instances (instance object)
  (let ((name (mesh-name (object-instance-mesh instance))))
    (when name
      (output "AttributeBegin~%")
      (rib-color (state-color instance))
      (rib-translate (state-location instance))
      (rib-scale (state-scale instance))
      (rib-rotate (state-orientation instance))
      (format-rib-texture-name (get-texture-name object))
      (output "ObjectInstance ~A~%" name)
      (output "AttributeEnd~%"))))

(defun rib-object (object)  
  (map-array (object-mesh-array object) #'rib-mesh)
  (mapc (lambda (instance) (rib-instances instance object))
	(object-instances object)))

(defun pov-rotation (rotation)
  (output "Rotate ~A 1.0 0.0 0.0~%" (- (point-y rotation)))
  (output "Rotate ~A 0.0 1.0 0.0~%" (- (point-x rotation))))

(defun fix-camera (point)
  (create-point
   (point-x point)
   (- (point-y point))
   (- (point-z point))))

(defun save-pov ()
  (rib-translate (fix-camera (transformation-translation *camera*)))
  (pov-rotation (transformation-rotation *camera*)))

(defun save-all ()
  (let ((*number* 1))
    (map-library #'rib-object)))

(defun save-rib (&optional (name "plant.rib") (pov "point-of-view.rib"))
  (file-safety-net pov #'save-pov)
  (file-safety-net name #'save-all))

(defun print-rib ()
  (let ((*stream* t))
    (save-all)))
