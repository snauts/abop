;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(defpackage #:povray
  (:use #:common-lisp #:abop))

(in-package #:povray)

(defvar *number* nil)
(export '*number*)

(defun point-povray (p &optional nz?)
  (format nil "<~6F,~6F~:[,~6F~;~*~]>" (point-x p) (point-y p) nz? (point-z p)))

(defun format-povray-color (c)
  (format nil "<~6F,~6F,~6F>" (color-r c) (color-g c) (color-b c)))

(defun povray-point (stream point accessor)
  (format stream "~A~%" (point-povray point (uv-p accessor)))
  (component-index accessor))  

(export 'save-component)
(defun save-component (vertex stream accessor point-save-fn)
  (write-component
   vertex accessor *data-base*
   (lambda (point) (funcall point-save-fn stream point accessor))))

(defun write-section (keyword number output)
  (when (> number 0)
    (let ((string (get-output-stream-string output)))
      (format *stream* "~A {~%~D,~%~A}~%" keyword number string))))

(export 'iterate-vertices)
(defun iterate-vertices (face-list fn)
  (dolist (face face-list)
    (dolist (vertex face)
      (funcall fn vertex))))

(defun povray-component-list (face-list keyword accessor)
  (with-output-to-string (output)
    (iterate-vertices
     face-list
     (lambda (vertex)
       (save-component vertex output accessor #'povray-point)))
    (write-section keyword (component-index accessor) output)))
    
(defun povray-all-components (faces)
  (let ((*index* (make-index -1)))
    (povray-component-list faces "vertex_vectors" #'vertex-position)
    (povray-component-list faces "normal_vectors" #'vertex-normal)
    (povray-component-list faces "uv_vectors" #'vertex-uv-map)))

(defvar *accessor* nil)
(defvar *index-count* nil)

(defun povray-index (vertex)
  (gethash (funcall *accessor* vertex) *data-base*))

(defun povray-triangle (stream triangle)
  (when (every *accessor* triangle)
    (format stream "<~{~D~^,~}>~%" (mapcar #'povray-index triangle))
    (incf *index-count*)))
  
(defun povray-make-pairs (list)
  (when (second list)
    (cons
     (list (elt list 0) (elt list 1))
     (povray-make-pairs (rest list)))))
    
(export 'facetize)
(defun facetize (polygon)
  (mapcar
   (lambda (pair) (cons (first polygon) pair))
   (povray-make-pairs (rest polygon))))

(defun povray-write-indices (faces keyword accessor-fn)
  (with-output-to-string (out)	
    (let ((*index-count* 0)
	  (*accessor* accessor-fn))
      (dolist (polygon faces)
	(dolist (triangle (facetize polygon))
	  (povray-triangle out triangle)))
      (write-section keyword *index-count* out))))

(defun povray-write-faces (faces)
  (povray-write-indices faces "face_indices" #'vertex-position)
  (povray-write-indices faces "normal_indices" #'vertex-normal)
  (povray-write-indices faces "uv_indices" #'vertex-uv-map))

(defun fix-name (name)
  (substitute #\_ #\- name))
       
(defun povray-texture (name)
  (when name
    (format *stream* "texture {~%")
    (format *stream* "  pigment { image_map { png \"~A\" } }~%" name)
    (format *stream* "  finish { ambient .5 }~%")
    (format *stream* "}~%")))

(defun povray-mesh (mesh texture)
  (let ((faces (mesh-faces mesh))
	(*data-base* (make-hash-table))
	(name (format nil "~A~:[~;~:*~D~]" (fix-name *name*) *number*)))
    (unless faces (return-from povray-mesh))
    (format *stream* "#declare ~A = mesh2 {~%" name)
    (setf (mesh-name mesh) name)
    (povray-all-components faces)
    (povray-write-faces faces)
    (format *stream* "uv_mapping~%")
    (povray-texture texture)
    (format *stream* "}~%")
    (when *number* (incf *number*))))

(defun povray-rotate (orientation)
  (multiple-value-bind (angle1 angle2 angle3)
      (get-transform-angles orientation)
    (format *stream* "rotate ~6F*y~%" (to-degrees angle3))
    (format *stream* "rotate ~6F*x~%" (to-degrees angle2))
    (format *stream* "rotate ~6F*y~%" (to-degrees angle1))))

(defun povray-scale (scale)
  (format *stream* "scale ~6F~%" scale))

(defun povray-translate (location)
  (format *stream* "translate ~A~%" (point-povray location)))

(defun povray-color (color)
  (format *stream* "pigment { rgb ~A }~%" (format-povray-color color)))

(defun povray-pigment (instance)
  (unless (get-texture-name (object-instance-object instance))
    (povray-color (state-color instance))))

(defun povray-instances (instance)
  (let ((name (mesh-name (object-instance-mesh instance))))
    (when name
      (format *stream* "object { ~A~%" name)
      (povray-rotate (state-orientation instance))
      (povray-scale (state-scale instance))
      (povray-translate (state-location instance))
      (povray-pigment instance)
      (format *stream* "}~%"))))

(defun povray-object (object)
  (let* ((mesh-array (object-mesh-array object))
	 (texture-name (get-texture-name object))
	 (*number* (if (= 1 (length mesh-array)) nil 0)))
    (map-array mesh-array (lambda (mesh) (povray-mesh mesh texture-name)))
    (mapc #'povray-instances (object-instances object))))

(defun povray-pov-translation ()
  (point-povray (transformation-translation *camera*)))

(defun povray-pov-rotation ()
  (point-povray (point-inv-x-y (transformation-rotation *camera*))))

(export 'save-povray)
(defun save-povray (&optional (name "scene.inc") (pov "point-of-view.inc"))
  (file-safety-net
   pov
   (lambda ()
     (format *stream* "translate ~A~%" (povray-pov-translation))
     (format *stream* "rotate ~A~%" (povray-pov-rotation))))
  (file-safety-net name (lambda () (map-library #'povray-object))))


