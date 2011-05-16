;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defstruct branch location color normal radius children)

(defstruct (turtle-cfg (:include state) (:type vector))
  (mesh 0)
  (branch nil)
  (vertical 0.0))

(defstruct (turtle (:include turtle-cfg) (:type vector))
  (stack nil))

(defvar *turtle* nil)
(defvar *branching* nil)

(defun turtle-set-radius (new-radius)
  (setf (state-scale *turtle*) (sfloat (abs new-radius))))

(defun turtle-set-surface (new-surface)
  (setf (state-scale *turtle*) (sfloat (sqrt (/ (abs new-surface) pi)))))

(defun turtle-update-orientation (torque angle)
  (setf (state-orientation *turtle*)
	(turn-space (state-orientation *turtle*) torque angle)))

(defun turn-turtle-around-axis (axis-fn angle)
  (turtle-update-orientation 
   (funcall axis-fn (state-orientation *turtle*))
   (to-radians angle)))

(defun turtle-pitch (angle)
  (turn-turtle-around-axis #'vector-space-x angle))

(defun turtle-roll (angle)
  (turn-turtle-around-axis #'vector-space-y angle))

(defun turtle-turn (angle)
  (turn-turtle-around-axis #'vector-space-z angle))

(defun turtle-flip-up ()
  (align-x-axis-with-horizon (state-orientation *turtle*)))

(defun get-normal (state)
  (vector-space-y (state-orientation state)))

(defun turtle-adjust (vector amount)
  (let ((torque (cross-product (point-normalize vector) (get-normal *turtle*))))
    (turtle-update-orientation torque (* amount (point-length torque)))))

(defun new-branch ()  
  (make-branch 
   :color (state-color *turtle*)
   :normal (get-normal *turtle*)
   :radius (state-scale *turtle*)
   :location (state-location *turtle*)))

(defun add-new-branch ()  
  (let ((branch (new-branch)))
    (if (null (turtle-branch *turtle*))
	(push branch *branching*)
	(push branch (branch-children (turtle-branch *turtle*))))
    (setf (turtle-branch *turtle*) branch)))

(defun finish-branch ()
  (setf (turtle-branch *turtle*) nil))

(defun turtle-displacement (amount)
  (point-add 
   (state-location *turtle*)
   (point-scale (get-normal *turtle*) amount)))

(defun turtle-move (amount)
  (setf (state-location *turtle*) (turtle-displacement amount)))

(defun turtle-forward (amount)
  (if (null (turtle-branch *turtle*)) (add-new-branch))
  (turtle-move amount)
  (add-new-branch))

(defun turtle-tropism (distance vector strength)
  (turtle-forward distance)
  (turtle-adjust vector strength))

(defun turtle-jump (amount)
  (turtle-move amount)
  (finish-branch))

(defun turtle-polygon-start ()
  (add-mesh "polygons" (state-color *turtle*))
  (polygon-start :name "polygons"))

(defun turtle-polygon-point ()
  (draw-vertex
   (make-vertex
    :position (state-location *turtle*)
    :normal (vector-space-z (state-orientation *turtle*)))))

(defun turtle-polygon-forward (x)
  (turtle-jump x)
  (turtle-polygon-point))

(defun turtle-polygon-finish ()
  (polygon-finish))

(defun turtle-teleport (point)
  (setf (state-location *turtle*) point)
  (finish-branch))

(defun turtle-push ()
  (push (copy-turtle *turtle*) (turtle-stack *turtle*)))

(defun turtle-pop ()
  (setf *turtle* (pop (turtle-stack *turtle*))))

(defun turtle-object (name &optional (scale 1.0) (color +white+))
  (turtle-push)
  (turtle-set-radius scale)
  (setf (state-color *turtle*) color)
  (put-object name *turtle*)
  (turtle-pop))

(defun turtle-set-color (new-color)
  (setf (state-color *turtle*) new-color))

(defvar *branch-map* nil)

(defun select-mesh (color)
  (setf-if-nil 
   (gethash color *branch-map*)
   (add-mesh "branching" color)))

(defun create-stub (dst src)
  (make-stub
   :location (branch-location src)
   :normal (branch-normal (or dst src))
   :radius (branch-radius (or dst src))))

(defun get-fatest (&optional branch1 branch2)
  (if (or (null branch2) (> (branch-radius branch1) (branch-radius branch2)))
      branch1
      branch2))

(defun branch-distance (branch1 branch2)
  (point-distance (branch-location branch1) (branch-location branch2)))

(defun vertical-increasement (branch child)
  (/ (branch-distance child branch) 
     (* 2 pi (branch-radius child))))

(defun calculate-branch-vertical (branch child)
  (+ *dst-vertical* 
     (if (and child (/= 0.0 (branch-radius child)))
	 (vertical-increasement branch child)
	 0.0)))

(defvar *next-vertical* 0.0)

(defun draw-branching (branch &optional old-stub)
  (when branch
    (let* ((*src-vertical* *dst-vertical*)
	   (*dst-vertical* *next-vertical*)
	   (mesh (select-mesh (branch-color branch)))
	   (fatest (reduce #'get-fatest (branch-children branch)))
	   (*next-vertical* (calculate-branch-vertical branch fatest))
	   (stub (draw-branch (create-stub fatest branch) old-stub mesh)))
      (setf (branch-children branch) (delete fatest (branch-children branch)))
      (when (branch-children branch) (draw-branching branch))
      (draw-branching fatest stub))))

(defun draw-all-branching ()
  (let ((*branch-map* (make-hash-table)))
    (mapc #'draw-branching *branching*)))

(defmacro with-turtle (&body body)
  `(let ((*turtle* (make-turtle)))
     (clean-up-forms
       (setf *turtle* nil)
       (setf *branching* nil))
     ,@body))
