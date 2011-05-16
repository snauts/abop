;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defconstant +default-zoom+ -1000.0s0)
(defconstant +default-height+ 300.0s0)

(export '*camera*)
(defparameter *camera* nil)

(export '*name*)
(defvar *name* nil)
(defvar *library* nil)

; Functions that rely on fact that vertex struct is list are:
; * create-store
; * parse-vertex
(def-exported-struct (vertex (:type list))
  position
  uv-map
  normal)

(export '(make-vertex vertex-position vertex-uv-map vertex-normal))

(def-exported-struct mesh
  has-uv
  opengl
  faces
  name)

(def-exported-struct object
  mesh-array
  instances
  texture)

(export '(object-mesh-array object-instances object-texture))

(defconstant +state-element-count+ 4)

(def-exported-struct (state (:type vector))
  (orientation (unit-space))
  (location (null-point))
  (color +white+)
  (scale 1.0))

(def-exported-struct (object-instance (:include state) (:type vector))
  object
  opengl
  mesh)

(def-exported-struct transformation
  translation
  rotation)

(export 'map-library)
(defun map-library (fn)
  (when *library*
    (maphash 
     (lambda (name object)
       (let ((*name* name))
	 (funcall fn object)))
     *library*)))

(defun new-array ()
  (make-array 0 :fill-pointer 0 :adjustable t :element-type 'list))

(defun add-empty-mesh (object)
  (vector-push-extend (make-mesh) (object-mesh-array object)))

(defun create-new-object (without-mesh)
  (let ((new-object (make-object :mesh-array (new-array))))
    (unless without-mesh (add-empty-mesh new-object))
    new-object))

(defvar *mesh* nil)
(defconstant +minimum-of-face-vertices+ 3)

(defun add-face (object face)
  (when (>= (length face) +minimum-of-face-vertices+)
    (let ((array (object-mesh-array object)))    
      (push face (mesh-faces (elt array (or *mesh* (1- (length array)))))))))

(defun probe-object (name)
  (gethash name *library*))

(defun get-object (name &key without-mesh)
  "Look for object in library, in case of absence create new."
  (setf-if-nil (gethash name *library*) (create-new-object without-mesh)))

(defun get-mesh (object num)
  (let ((array (object-mesh-array object)))
    (aref array (mod num (length array)))))

(defun replace-state (state-derivative state)
  (replace state-derivative state :end2 +state-element-count+))

(defun create-object-instance (object mesh)
  (make-object-instance :object object :mesh (get-mesh object mesh)))

(defun add-instance (object mesh state)
  (push (replace-state (create-object-instance object mesh) state)
	(object-instances object)))

(defun put-object (name state &optional (mesh 0))
  (let ((object (probe-object name)))
    (when object (add-instance object mesh state))))

(defun get-fresh-camera ()
  (make-transformation
   :rotation (create-point 0 0 0)
   :translation (create-point 0 +default-height+ +default-zoom+)))

(defmacro with-library (&body body)
  `(let ((*library* (make-hash-table :test #'equal))
	 (*camera* (get-fresh-camera)))
     ,@body))

(defun add-mesh (name color)
  (let* ((object (get-object name :without-mesh t))
	 (mesh-number (add-empty-mesh object)))
    (add-instance object mesh-number (make-state :color color))
    mesh-number))

; --- draw polygon ------------------------------------------------------------

(defparameter *polygon-stack* nil)

(defun polygon-start (&key name)
  (push (cons (get-object name) nil) *polygon-stack*))

(defun draw-vertex (vertex)
  (push vertex (cdr (first *polygon-stack*))))

(defun polygon-finish ()
  (let ((polygon (pop *polygon-stack*)))
    (add-face (car polygon) (cdr polygon))))

; --- draw cylinder -----------------------------------------------------------

(defconstant +default-segments+ 6)

(defvar *segments* nil)

(defun make-segments (count)
  (loop for i from 1 to count collect (/ (* 2 (1- i) pi) count)))

(defun segment-points (count)
  (loop for i from 0 to count collect (create-point (/ i count) 0.0 0.0)))

(defun make-uv-database (count)
  (make-array (1+ count) :initial-contents (segment-points count)))

(defvar *uv-database* nil)

(defmacro with-segments (&body body)
  `(let ((*uv-database* (make-uv-database +default-segments+))
	 (*segments* (make-segments +default-segments+)))
     ,@body))

(defun init-segments (&key (count +default-segments+))
  (when *uv-database* (setf *uv-database* (make-uv-database count)))
  (when *segments* (setf *segments* (make-segments count))))

(defstruct stub location radius normal dots vertices)

(defvar *stub* nil)

(defun make-position (arm)
  (point-add (stub-location *stub*) (point-scale arm (stub-radius *stub*))))

(defun make-stub-vertex (point)
  (let ((arm (point-sub point (stub-location *stub*))))
    (make-vertex :normal (point-normalize arm) :position (make-position arm))))

(defun make-vertices (dots &optional 1st)
  (if (null dots)
      (cons (copy-vertex 1st) nil)      
      (let ((vertex (make-stub-vertex (first dots))))
	(cons (if 1st vertex (setf 1st vertex))
	      (make-vertices (rest dots) 1st)))))

(defun get-vertices (stub)
  (let ((*stub* stub))
    (setf-if-nil (stub-vertices stub) (make-vertices (stub-dots stub)))))

(defvar *uv-index* nil)

(defun default-uv-calc (radius vertical) 
  (declare (ignore radius))
  vertical)

(defparameter *uv-calc* #'default-uv-calc)

(defun set-uv-calculator (fn)
  (setf *uv-calc* fn)
  (clean-up-forms 
    (setf *uv-calc* #'default-uv-calc)))

(defun draw-cylinder-vertex (vertex radius vertical)
  (when (null (vertex-uv-map vertex))
    (let ((uv (copy-point (aref *uv-database* *uv-index*))))
      (setf (point-y uv) (sfloat (funcall *uv-calc* radius vertical)))
      (setf (vertex-uv-map vertex) uv)))
  (draw-vertex vertex))

(defvar *src-radius* nil)
(defvar *dst-radius* nil)

(defvar *src-vertical* 0.0)
(defvar *dst-vertical* 0.0)

(defun draw-cylinder-face (src dst)
  (when (and (rest src) (rest dst))
    (polygon-start :name "branching")
    (draw-cylinder-vertex (first src) *src-radius* *src-vertical*)
    (draw-cylinder-vertex (first dst) *dst-radius* *dst-vertical*)
    (incf *uv-index*)
    (draw-cylinder-vertex (second dst) *dst-radius* *dst-vertical*)
    (draw-cylinder-vertex (second src) *src-radius* *src-vertical*)
    (polygon-finish)))

(defun draw-cylinder (src dst)
  (let ((*uv-index* 0)
	(*src-radius* (stub-radius src))
	(*dst-radius* (stub-radius dst)))
    (mapl #'draw-cylinder-face (get-vertices src) (get-vertices dst)))
  dst)

(defun calculate-perpendicular (x y)
  (let ((length (sqrt (+ (expt x 2) (expt y 2)))))
    (create-point (/ (- y) length) (/ x length) 0.0)))

(defun perpendicular (direction)
  (if (= 0.0 (point-x direction))
      (create-point 1.0 0.0 0.0)
      (calculate-perpendicular (point-x direction) (point-y direction))))

(defun transform-arm (stub arm angle &optional (scale 1.0))
  (setf arm (rotate-vector (copy-point arm) (stub-normal stub) (- angle)))
  (point-add (stub-location stub) (point-scale arm scale)))

(defun circle-shape (stub arm)
  (mapcar (lambda (point) (transform-arm stub arm point)) *segments*))

(defun set-stub-dots (stub value)
  (setf (stub-dots stub) value)
  stub)

(defun init-stub-dots (stub)
  (let ((arm (perpendicular (stub-normal stub))))
    (set-stub-dots stub (circle-shape stub arm))))

(defun inter-normal (n1 n2)
  (point-normalize
   (if (not (point-eq n1 (point-invert n2))) 
       (point-add n1 n2)
       (point-sub n1 n2))))

(defun create-stub-plane (next-stub prev-stub)
  (create-plane 
   (stub-location next-stub)
   (inter-normal
    (stub-normal next-stub)
    (stub-normal prev-stub))))

(defun dots-projected-in-plane (plane prev-stub)
  (loop for x in (stub-dots prev-stub)
     collect (plane-intersection x (stub-normal prev-stub) plane)))

(defun produce-dots (next-stub prev-stub)
  (let ((plane (create-stub-plane next-stub prev-stub)))
    (set-stub-dots next-stub (dots-projected-in-plane plane prev-stub))))

(defun draw-branch (next-stub prev-stub mesh-number)
  (let ((*mesh* mesh-number))
    (if (not prev-stub)
	(init-stub-dots next-stub)
	(draw-cylinder prev-stub (produce-dots next-stub prev-stub)))))