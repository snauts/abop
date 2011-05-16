;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defun assign-texture (name texture-name)
  (setf (object-texture (get-object name)) texture-name))

(defun get-texture (object)
  (let ((texture-id (object-texture object)))
    (if (consp texture-id) (rest texture-id) 0)))

(cffi:defcfun ("load_texture" load_texture) :unsigned-int
  (name :string))

(cffi:defcfun ("delete_texture" delete_texture) :void
  (texture :unsigned-int))

(defun make-texture-setfer (fn &key test)
  (lambda (object)
    (when (funcall test (object-texture object))
      (setf (object-texture object) (funcall fn (object-texture object))))))

(export 'get-texture-name)
(defun get-texture-name (object)
  (let ((texture (object-texture object)))
    (cond ((stringp texture) texture)
	  ((consp texture) (first texture)))))

(defun get-full-path (name)
  (if (eq #\/ (elt name 0))
      name
      (concatenate 'string (namestring *default-pathname-defaults*) name)))

(defun load-one-texture (name)
  (cons name (load_texture (get-full-path name))))

(defun load-textures ()
  (map-library (make-texture-setfer #'load-one-texture :test #'stringp)))

(defun delete-one-texture (texture)
  (delete_texture (rest texture)))

(defun delete-textures ()
  (map-library (make-texture-setfer #'delete-one-texture :test #'consp)))

(defun opengl-point (fn point)
  (when point
    (funcall fn (point-x point) (point-y point) (point-z point))))

(defun opengl-uv (point)
  (when point
    (%gl:tex-coord-2d (point-x point) (- 1.0 (point-y point)))))

(defun opengl-color (color)
  (when color
    (%gl:color-4f
     (color-r color)
     (color-g color)
     (color-b color)
     (color-a color))))

(defun opengl-vertex (vertex)
  (opengl-uv (vertex-uv-map vertex))
  (opengl-point #'%gl:normal-3d (vertex-normal vertex))
  (opengl-point #'%gl:vertex-3d (vertex-position vertex)))

(defun opengl-face (face)
  (gl:begin :triangle-fan)
  (mapc #'opengl-vertex face)
  (gl:end))

(defun opengl-translate (point)
  (%gl:translate-d (point-x point) (point-y point) (point-z point)))

(defun opengl-scale (scale)
  (%gl:scale-d scale scale scale))

(defun opengl-rotate (angle vector)
  (%gl:rotate-d
   (to-degrees angle)
   (point-x vector)
   (point-y vector)
   (point-z vector)))

(defun win-fix-acos (x)
  (cond ((= x +1.0) 0.0)
	((= x -1.0) pi)	
	(t (acos x))))

(defun angle3 (side back)
  (- pi (atan (point-y side) (point-y back))))

(export 'get-transform-angles)
(defun get-transform-angles (orientation)
  (let* ((side (vector-space-x orientation))
	 (head (vector-space-y orientation))
         (back (vector-space-z orientation))
         (angle1 (atan (point-x head) (point-z head)))
	 (angle2 (win-fix-acos (min 1.0 (max -1.0 (point-y head))))))
    (if (float-equal 1.0 (abs (point-y head)))
	(values (atan (point-z side) (point-z back)) angle2 0.0)
	(values angle1 angle2 (angle3 side back)))))

(defun opengl-rotations (orientation)
  (multiple-value-bind (angle1 angle2 angle3)
      (get-transform-angles orientation)
    (opengl-rotate angle1 +y-axis+)
    (opengl-rotate angle2 +x-axis+)
    (opengl-rotate angle3 +y-axis+)))

(defun opengl-display-list (commands)
  (let ((list-id (gl:gen-lists 1)))
    (gl:new-list list-id :compile)
    (funcall commands)
    (gl:end-list)
    list-id))  

(defun make-instance-display-list (instance)
  (opengl-display-list
   (lambda ()
     (opengl-translate (state-location instance))
     (opengl-rotations (state-orientation instance))
     (opengl-scale (sfloat (state-scale instance)))
     (opengl-color (state-color instance)))))

(defun get-instance-display-list (instance)
  (setf-if-nil
   (object-instance-opengl instance)
   (make-instance-display-list instance)))
  
(defun make-mesh-display-list (mesh)
  (opengl-display-list (lambda () (mapc #'opengl-face (mesh-faces mesh)))))

(defun get-mesh-display-list (mesh)
  (setf-if-nil (mesh-opengl mesh) (make-mesh-display-list mesh)))

(defun opengl-instance (instance)
  (gl:push-matrix)
  (gl:call-list (get-instance-display-list instance))
  (gl:call-list (get-mesh-display-list (object-instance-mesh instance)))
  (gl:pop-matrix))
  
(defvar *command-list* nil)

(defun produce-opengl-instances (object)
  (gl:bind-texture :texture-2d (get-texture object))
  (mapc #'opengl-instance (object-instances object))
  (gl:bind-texture :texture-2d 0))

(defun free-display-list (id)
  (when (integerp id) (gl:delete-lists id 1)))

(defun free-instance (instance)
  (free-display-list (object-instance-opengl instance)))

(defun free-mesh (mesh)
  (free-display-list (mesh-opengl mesh)))

(defun free-object (object)
  (mapc #'free-instance (object-instances object))
  (map-array (object-mesh-array object) #'free-mesh))

(defun free-all-display-lists ()
  (map-library #'free-object))

(defun produce-opengl-commands ()
  (lambda ()
    (map-library #'produce-opengl-instances)
    (gl:flush)))
