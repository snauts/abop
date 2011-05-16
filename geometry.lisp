;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(cffi:define-foreign-library machine 
    (:windows "./machine.dll")
    (:unix "./machine.so"))
   
(cffi:use-foreign-library machine)

(cffi:defcfun ("cseed" cseed) :void
  (seed :unsigned-int))

(cffi:defcfun ("crandom" crandom) :int
  (seed :unsigned-int))

;; --- point ------------------------------------------------------------------

(defconstant +x+ 0)
(defconstant +y+ 1)
(defconstant +z+ 2)

(defconstant +dimensions+ 3)

(def-exported-struct (point (:type vector))
  (x     0.0s0 :type single-float)
  (y     0.0s0 :type single-float)
  (z     0.0s0 :type single-float)
  (size -1.0s0 :type single-float))

(export 'create-point)
(defun create-point (x y z)
  (make-point :x (sfloat x) :y (sfloat y) :z (sfloat z)))

(defun point-index (point i)
  (svref point i))

(declaim (inline setf-point-index))

(defun setf-point-index (point i element)
  (setf (svref point i) element))

(defsetf point-index setf-point-index)

; --- point data abstraction ---

(defun point-addf (a b)
  (incf (point-x a) (point-x b))
  (incf (point-y a) (point-y b))
  (incf (point-z a) (point-z b))
  (setf (point-size a) -1.0s0)
  a)

(defun point-eq (a b)
  (and (float-equal (point-x a) (point-x b))
       (float-equal (point-y a) (point-y b))
       (float-equal (point-z a) (point-z b))))

(export 'point-add)
(defun point-add (a b)
  (point-addf (copy-point a) b))

(export 'point-scale)
(defun point-scale (p q)
  (create-point (* q (point-x p)) (* q (point-y p)) (* q (point-z p))))

(defun point-sum (point)
  (+ (point-x point) (point-y point) (point-z point)))

(defun calculate-point-length (p)
  (sqrt (+ (expt (point-x p) 2) (expt (point-y p) 2) (expt (point-z p) 2))))

(defun point-length (p)
  (if (minusp (point-size p))
      (setf (point-size p) (calculate-point-length p))
      (point-size p)))

(export 'point-invert)
(defun point-invert (p)
  (create-point (- (point-x p)) (- (point-y p)) (- (point-z p))))

(defun point-sub (a b)
  (create-point
   (- (point-x a) (point-x b))
   (- (point-y a) (point-y b))
   (- (point-z a) (point-z b))))

(defun point-normalize (p &optional (size 1.0))
  (point-scale p (/ size (point-length p))))

(export 'point-write)
(defun point-write (p &optional nz?)
  (format nil "~6F ~6F ~:[~6F~;~*~]" (point-x p) (point-y p) nz? (point-z p)))

(export 'point-inv-x-y)
(defun point-inv-x-y (p)
  (create-point (point-y p) (point-x p) (point-z p)))

(defun point-read (stream &optional ignore-z?)
  (create-point (read stream) (read stream) (if ignore-z? 0.0 (read stream))))

(defun point-distance (a b)
  (point-length (point-sub a b)))

(defun null-point ()
  (create-point 0.0 0.0 0.0))

(defparameter +null-point+ (null-point))

(cffi:defcfun ("rotate_vector" rotate_vector) :void
  (vector :pointer)
  (axis :pointer)
  (angle :float))

(defun point-import (array point)
  (dotimes (i +dimensions+)
    (setf (point-index point i) (cffi:mem-aref array :float i)))
  point)

(defun point-export (point array)
  (dotimes (i +dimensions+)
    (setf (cffi:mem-aref array :float i) (point-index point i))))

(defmacro with-foreign-point ((ptr var) &rest body)  
  `(cffi:with-foreign-object (,ptr :float +dimensions+)
     (point-export ,var ,ptr)
     ,@body))

(defun rotate-vector (vector axis angle)
  (with-foreign-point (foreign-vector vector)
    (with-foreign-point (foreign-axis axis)
      (rotate_vector foreign-vector foreign-axis (sfloat angle))
      (point-import foreign-vector vector))))

(defun cross-product (a b)
  (create-point
   (- (* (point-y a) (point-z b)) (* (point-z a) (point-y b)))
   (- (* (point-z a) (point-x b)) (* (point-x a) (point-z b)))
   (- (* (point-x a) (point-y b)) (* (point-y a) (point-x b)))))

(defun dot-product (a b)
  (+ (* (point-x a) (point-x b))
     (* (point-y a) (point-y b))
     (* (point-z a) (point-z b))))

(defun get-angle-cos (a b)
  (/ (dot-product a b) (* (point-length a) (point-length b))))

(defun safe-acos (val)
  (cond ((= +1.0 val) 0.0)
	((= -1.0 val) pi)
	(t (acos val))))

(defun get-angle (a b)
  (let ((result (safe-acos (get-angle-cos a b))))
    (if (complexp result)
	(realpart result)
	result)))
    
;; --- space ------------------------------------------------------------------

(defconstant +space-elements+ (expt +dimensions+ 2))

(defstruct (vector-space (:type vector)) x y z)

(defun space-element (space index)
  (multiple-value-bind (axis-index dimension-index) (floor index +dimensions+)
    (point-index (elt space axis-index) dimension-index)))

(defparameter +x-axis+ (create-point 1.0 0.0 0.0))
(defparameter +y-axis+ (create-point 0.0 1.0 0.0))
(defparameter +z-axis+ (create-point 0.0 0.0 1.0))

(defun unit-space ()
  (make-vector-space
   :x (copy-point +x-axis+)
   :y (copy-point +y-axis+)
   :z (copy-point +z-axis+)))

(defun turn-space (space torque angle)
  (flet ((rotate-axis (axis) (rotate-vector (copy-point axis) torque angle)))
    (map 'vector #'rotate-axis space)))

(cffi:defcfun ("rotate_space" rotate_space) :void
  (vector :pointer)
  (space :pointer))

(defun export-space (space array)
  (dotimes (i +space-elements+)
    (setf (cffi:mem-aref array :float i) (space-element space i))))

(export 'rotate-space)
(defun rotate-space (vector space)
  (with-foreign-point (foreign-vector vector)
    (cffi:with-foreign-object (foreign-space :float +space-elements+)
      (export-space space foreign-space)
      (rotate_space foreign-vector foreign-space)
      (point-import foreign-vector (null-point)))))

(defun normal-cross-product (vector1 vector2)
  (point-normalize (cross-product vector1 vector2)))

(defun align-x-axis-with-horizon (space)
  (let ((space-y (vector-space-y space)))
    (when (< single-float-epsilon (point-distance +y-axis+ space-y))
      (setf (vector-space-x space)
	    (normal-cross-product +y-axis+ space-y))
      (setf (vector-space-z space)
	    (normal-cross-product (vector-space-x space) space-y)))))

;; --- plane ------------------------------------------------------------------

; plane equation A * x + B * y + C * z + D = 0
; (A, B, C) = normal, D = distance
(defstruct plane normal distance)

(defun create-plane (point normal)
  (make-plane :normal normal :distance (- (dot-product point normal))))

(defun distance-factor (p v plane)
  "distance from point 'p' to plane mesured in 'v' vectors"
  (/ (- (+ (dot-product (plane-normal plane) p) (plane-distance plane)))
     (dot-product (plane-normal plane) v)))

(defun plane-intersection (p v plane)
  "intersection of plane and line formed by point 'p' and vector 'v'"
  (point-add p (point-scale v (distance-factor p v plane))))

;; --- color ------------------------------------------------------------------

(declaim (inline create-color))

(def-exported-struct color
  (r 0.0s0 :type single-float)
  (g 0.0s0 :type single-float)
  (b 0.0s0 :type single-float)
  (a 0.0s0 :type single-float))

(defun create-color (r g b &optional (a 1.0))
  (make-color :r (sfloat r) :g (sfloat g) :b (sfloat b) :a (sfloat a)))

(export 'color-scale)
(defun color-scale (color q)
  (create-color
   (* q (color-r color))
   (* q (color-g color))
   (* q (color-b color))
   (color-a color)))

(defparameter +white+ (create-color 1.0 1.0 1.0))
(defparameter +black+ (create-color 0.0 0.0 0.0))
(defparameter +red+ (create-color 1.0 0.0 0.0))
