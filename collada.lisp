;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(defpackage #:collada
  (:use #:common-lisp #:abop #:povray))

(in-package #:collada)

(defvar *effects* nil)
(defvar *xml-indent* nil)

(defconstant +scene-scale+ 0.05)

(defun phash (something)
  (format nil "#~A" something))

(defun format-attribute (attribute)
  (format nil " ~A=\"~A\"" (first attribute) (second attribute)))

(defun expand (attributes)
  (apply #'concstrs (mapcar #'format-attribute attributes)))

(defun xml-tag (name attributes body-fn)
  (format *stream* "~VT<~A~A>~%" *xml-indent* name (expand attributes))
  (incf *xml-indent*)
  (funcall body-fn)
  (decf *xml-indent*)
  (format *stream* "~VT</~A>~%" *xml-indent* name))

(defmacro block-tag (name attributes &body body)
  `(xml-tag ,name ,attributes (lambda () ,@body)))

(defmacro simple-block-tag (name &body body)
  `(block-tag ,name nil ,@body))

(defun line-tag (name attributes text)
  (format *stream* "~VT<~A~A>" *xml-indent* name (expand attributes))
  (format *stream* "~A</~A>~%" text name))

(defun simple-line-tag (name text)
  (line-tag name nil text))

(defun just-tag (name attributes)
  (format *stream* "~VT<~A~A/>~%" *xml-indent* name (expand attributes)))

(defun save-camera ()
  (simple-block-tag "library_cameras"
    (block-tag "camera" '(("id" "camera"))
      (simple-block-tag "optics"
	(simple-block-tag "technique_common"
	  (simple-block-tag "perspective"
	    (simple-line-tag "xfov" "58")
	    (simple-line-tag "znear" "0.1")
	    (simple-line-tag "zfar" "10000.0")
	    (simple-line-tag "aspect_ratio" "1.33333")))))))

(defun save-light ()
  (simple-block-tag "library_lights"
    (block-tag "light" '(("id" "light"))
      (simple-block-tag "technique_common"
	(simple-block-tag "point"
	  (simple-line-tag "color" "1.0 1.0 1.0")
	  (simple-line-tag "constant_attenuation" "1.0")
	  (simple-line-tag "linear_attenuation" "0.0")
	  (simple-line-tag "quadratic_attenuation" "0.0"))))))

(defun point-collada (p &optional stream nz? ns?)
  (if (null ns?) (setf p (point-scale p +scene-scale+)))
  (format stream "~F ~F ~:[~F ~;~*~]" (point-x p) (point-y p) nz? (point-z p)))

(defun should-not-scale? (accessor)
  (not (eq accessor #'vertex-position)))

(defun point (stream point accessor)
  (point-collada point stream (uv-p accessor) (should-not-scale? accessor))
  (component-index accessor))

(defun component-count (accessor)
  (if (uv-p accessor) 2 3))

(defun accessor-block-parameters (names)
  (dolist (i names)
    (line-tag "param" `(("type" "float") ("name" ,i)) "")))

(defparameter *uv-param-names* (list "S" "T"))
(defparameter *xyz-param-names* (list "X" "Y" "Z"))
 
(defun accessor-block (name accessor)
  (simple-block-tag "technique_common" 
    (block-tag "accessor" `(("count" ,(1+ (funcall accessor *index*)))
			    ("source" ,(concstrs "#" name "-array"))
			    ("stride" ,(component-count accessor)))
      (accessor-block-parameters
       (if (uv-p accessor)
	   *uv-param-names*
	   *xyz-param-names*)))))

(let ((data (make-vertex :position "position" :normal "normal" :uv-map "uv")))
  (defun get-component-name (accessor)
    (funcall accessor data)))

(defun float-array-attributes (name accessor)
  `(("count" ,(* (1+ (funcall accessor *index*)) (component-count accessor)))
    ("id" ,(concstrs name "-array"))))

(defun collada-save-component (output accessor name)
  (setf name (format nil "~A-~A" name (get-component-name accessor)))
  (block-tag "source" `(("id" ,name))
    (line-tag "float_array"
	      (float-array-attributes name accessor)
	      (get-output-stream-string output))
    (accessor-block name accessor)))

(defun float-array (mesh accessor)
  (with-output-to-string (output)
    (let ((*index* (make-index -1))
	  (should-save-component t))
      (iterate-vertices
       (mesh-faces mesh)
       (lambda (vertex)
	 (save-component vertex output accessor #'point)))
      (when (uv-p accessor)
	(setf (mesh-has-uv mesh) (>= (funcall accessor *index*) 0))
	(setf should-save-component (mesh-has-uv mesh)))
      (when should-save-component
	(collada-save-component output accessor (mesh-name mesh))))))

(defun input-block (offset semantic mesh-name extension)
  (let ((attributes `(("semantic" ,semantic)
		      ("source" ,(concstrs "#" mesh-name "-" extension)))))
    (when offset (push `("offset" ,offset) attributes))
    (just-tag "input" attributes)))

(defun mesh-vertice-block (name)
  (block-tag "vertices" `(("id" ,(concstrs name "-vertex")))
    (input-block nil "POSITION" name "position")))

(defun look-up-indice (key)
  (gethash key *data-base*))

(defun save-vertex-indices (output vertex)
  (format output "~A ~A ~:[~;~:*~A ~]"
	  (look-up-indice (vertex-position vertex))
	  (look-up-indice (vertex-normal vertex))
	  (look-up-indice (vertex-uv-map vertex))))

(defun triangles (output mesh)
  (let ((count 0))
    (dolist (face (mesh-faces mesh) count)
      (dolist (triangle (facetize face))
	(dolist (vertex triangle)
	  (save-vertex-indices output vertex))
	(incf count)))))

(defun mesh-polygon-block (mesh)
  (with-output-to-string (output)
    (block-tag "triangles" `(("count" ,(triangles output mesh)))
      (input-block 0 "VERTEX" (mesh-name mesh) "vertex")
      (input-block 1 "NORMAL" (mesh-name mesh) "normal")
      (when (mesh-has-uv mesh)
	(input-block 2 "TEXCOORD" (mesh-name mesh) "uv"))
      (simple-line-tag "p" (get-output-stream-string output)))))

(defun save-mesh (mesh)
  (when (mesh-faces mesh)
    (setf (mesh-name mesh) (format nil "~A~:[~;~:*~D~]-mesh" *name* *number*))
    (block-tag "geometry" `(("id" ,(mesh-name mesh)))
      (simple-block-tag "mesh"
	(let ((*data-base* (make-hash-table)))
	  (float-array mesh #'vertex-position)
	  (float-array mesh #'vertex-normal)
	  (float-array mesh #'vertex-uv-map)
	  (mesh-vertice-block (mesh-name mesh))
	  (mesh-polygon-block mesh)
	  (when *number* (incf *number*)))))))
  
(defun save-geometry ()
  (simple-block-tag "library_geometries"
    (map-library
     (lambda (object)
       (let* ((mesh-array (object-mesh-array object))
	      (*number* (if (= 1 (length mesh-array)) nil 0)))
	 (map-array mesh-array #'save-mesh))))))

(defun effect-phong (diffuse-printer ambient-color)
  (block-tag "technique" '(("sid" "common"))
    (simple-block-tag "phong"
      (simple-block-tag "emission"
	(simple-line-tag "color" "0.0 0.0 0.0 1.0"))
      (simple-block-tag "ambient"
	(simple-line-tag "color" ambient-color))
      (simple-block-tag "diffuse"
	(funcall diffuse-printer))
      (simple-block-tag "specular"
	(simple-line-tag "color" "0.5 0.5 0.5 1.0"))
      (simple-block-tag "shininess"
	(simple-line-tag "float" "10"))
      (simple-block-tag "reflective"
	(simple-line-tag "color" "1.0 1.0 1.0 1.0"))
      (simple-block-tag "reflectivity"
	(simple-line-tag "float" "0.0"))
      (simple-block-tag "transparent"
	(simple-line-tag "color" "1.0 1.0 1.0 1.0"))
      (simple-block-tag "transparency"
	(simple-line-tag "float" "0.0")))))

(defun fix-point (point)
  (point-scale
   (create-point
    (- (point-x point))
    (+ (point-y point))
    (- (point-z point)))
   +scene-scale+))

(defun camera-translation (translation)
  (simple-line-tag "translate" (point-write (fix-point translation))))

(defun angle-to-str (angle &optional prefix)
  (format nil "~:[~;~:*~A ~]~6F" prefix angle))

(defun camera-rotation (rotation)
  (simple-line-tag "rotate" (angle-to-str (- (point-x rotation)) "0 1 0"))
  (simple-line-tag "rotate" (angle-to-str (- (point-y rotation)) "1 0 0")))

(defun camera-transformation (transformation)
  (camera-rotation (transformation-rotation transformation))
  (camera-translation (transformation-translation transformation))
  (simple-line-tag "rotate" "1 0 0 90.0")
  (simple-line-tag "rotate" "0 1 0 90.0"))
  
(defun instance-scale (instance)
  (let ((scale (state-scale instance)))
    (format nil "~F ~F ~F" scale scale scale)))

(defun instance-rotation (instance)
  (multiple-value-bind (angle1 angle2 angle3)
      (get-transform-angles (state-orientation instance))
    (simple-line-tag "rotate" (angle-to-str (to-degrees angle1) "0 1 0"))
    (simple-line-tag "rotate" (angle-to-str (to-degrees angle2) "1 0 0"))
    (simple-line-tag "rotate" (angle-to-str (to-degrees angle3) "0 1 0"))))

(defun instance-material (instance)
  (let* ((object (object-instance-object instance))
	 (texture (object-texture object)))
    (if texture
	(assoc (get-texture-name object) *effects*)
	(assoc (state-color instance) *effects*))))

(defun material-attributes (number)
  (let ((name (format nil "material-~A" number)))
    `(("symbol" ,name)
      ("target" ,(phash name)))))

(defun format-bind-vertex-input (name)
  (just-tag "bind_vertex_input" `(("input_semantic" "TEXCOORD")
				  ("semantic" ,(concstrs name "-channel")))))

(defun bind-material (mesh instance)
  (let ((info (rest (instance-material instance))))
    (block-tag "instance_geometry" `(("url" ,(phash (mesh-name mesh))))
      (simple-block-tag "bind_material"
	(simple-block-tag "technique_common"
	  (block-tag "instance_material" (material-attributes (first info))
	    (when (stringp (rest info))
	      (format-bind-vertex-input (rest info)))))))))

(defun save-object-instance (instance)
  (let ((mesh (object-instance-mesh instance)))
    (when (mesh-faces mesh)
      (simple-block-tag "node"
	(simple-line-tag "translate" (point-collada (state-location instance)))
	(instance-rotation instance)
	(simple-line-tag "scale" (instance-scale instance))
	(bind-material mesh instance)))))

(defun save-all-instances ()
  (map-library
   (lambda (object)
     (mapcar #'save-object-instance (object-instances object)))))

(defun material-fx-name (number)
  (format nil "material-~A-fx" number))

(defun effect-wrapper (item fn)
  (when (not (assoc item *effects*))
    (block-tag "effect" `(("id" ,(material-fx-name (incf *number*))))
      (simple-block-tag "profile_COMMON"
	(setf item (cons item (cons *number* (funcall fn item))))))
    (push item *effects*)))

(defun format-texture (texture-name)
  (just-tag "texture" `(("texcoord" ,(concstrs texture-name "-channel"))
			("texture" ,(concstrs texture-name "-sampler")))))

(defun texture-phong (name)
  (setf name (substitute #\_ #\. name))
  (block-tag "newparam" `(("sid" ,name))
    (block-tag "surface" '(("type" "2D"))
      (simple-line-tag "init_from" (concstrs name "-img"))
      (simple-line-tag "format" "A8R8G8B8")))
  (block-tag "newparam" `(("sid" ,(concstrs name "-sampler")))
    (simple-block-tag "sampler2D"
      (simple-line-tag "source" name)
      (simple-line-tag "minfilter" "LINEAR_MIPMAP_LINEAR")
      (simple-line-tag "magfilter" "LINEAR")))
  (effect-phong
   (lambda () (format-texture name))
   "0.4 0.4 0.4 1.0")
  name)

(defun save-texture (object)
  (let ((name (get-texture-name object)))
    (effect-wrapper name #'texture-phong)))

(defun format-color (color)
  (format nil "~F ~F ~F ~F"
	  (color-r color)
	  (color-g color)
	  (color-b color)
	  (color-a color)))

(defun color-phong (color)
  (effect-phong 
   (lambda () (simple-line-tag "color" (format-color color)))
   (format-color (color-scale color 0.4)))
  color)

(defun save-color (object)
  (mapc
   (lambda (object-instance)
     (let ((color (state-color object-instance)))
       (effect-wrapper color #'color-phong)))
   (object-instances object)))

(defun save-effects ()
  (let ((*number* -1))
    (simple-block-tag "library_effects"
      (map-library
       (lambda (object)
	 (if (object-texture object)
	     (save-texture object)
	     (save-color object)))))))

(defun save-library (name fn)
  (simple-block-tag name
    (mapc fn *effects*)))

(defun save-images (effect)
  (let ((effect-name (rest (rest effect))))
    (when (stringp effect-name)
      (block-tag "image" `(("id" ,(concstrs effect-name "-img")))
	(simple-line-tag "init_from" (first effect))))))

(defun save-materials (effect)
  (let ((i (first (rest effect))))
    (block-tag "material" `(("id" ,(format nil "material-~A" i)))
      (just-tag "instance_effect" `(("url" ,(phash (material-fx-name i))))))))

(defun view-point-node (transformation name)
  (simple-block-tag "node"
    (camera-transformation transformation)
    (just-tag (concstrs "instance_" name) `(("url" ,(phash name))))))

(defun save-scene ()
  (simple-block-tag "library_visual_scenes"
    (block-tag "visual_scene" '(("id" "scene"))
      (save-all-instances)
      (view-point-node *camera* "camera")
      (view-point-node *camera* "light")))
  (simple-block-tag "scene"
    (just-tag "instance_visual_scene" '(("url" "#scene")))))

(export 'save-collada)
(defun save-collada (&optional (name "scene.dae"))
  (file-safety-net
   name
   (lambda ()
     (let ((*xml-indent* 1)
	   (*effects* nil))
       (format *stream* "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%")       
       (block-tag "COLLADA" '(("version" "1.4.0"))
	 (simple-block-tag "asset"
	   (just-tag "unit" '(("meter" "0.01")))
	   (simple-line-tag "up_axis" "Y_UP"))
	 (save-camera)
	 (save-effects)
	 (save-light)
	 (save-library "library_images" #'save-images)
	 (save-library "library_materials" #'save-materials)
	 (save-geometry)
	 (save-scene))))))
