;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defvar *anti-aliasing* nil)

(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)

(defparameter *draw-command* nil)

(cffi:defcfun ("get_w" get_w) :float)
(cffi:defcfun ("get_h" get_h) :float)

(defun update-commands ()
  (setf *draw-command* (produce-opengl-commands)))

(defun with-float-array (fn &rest values)
  (let ((size (length values)))
    (cffi:with-foreign-object (array :float size)
      (dotimes (i size)
	(setf (cffi:mem-aref array :float i) (elt values i)))
      (funcall fn array))))

(defun setup-light ()
  (with-float-array
      (lambda (light-pos) (%gl:light-fv :light0 :position light-pos))
    0.0 0.0 1000.0 0.0)
  
  (with-float-array
      (lambda (light-color) (%gl:light-fv :light0 :ambient light-color))
    0.2 0.2 0.2 0.0)
  
  (with-float-array
      (lambda (light-color) (%gl:light-fv :light0 :diffuse light-color))
    0.6 0.6 0.6 1.0))

(defun change-mode (mode)
  (gl:matrix-mode mode)
  (gl:load-identity))

(defun orthogonal ()
  (change-mode :projection)
  (glu:ortho-2d 0.0s0 (get_w) (get_h) 0.0s0)
  (change-mode :modelview))

(defun opengl-draw-mouse ()
  (orthogonal)
  (gl:clear :depth-buffer-bit)
  (gl:point-size 5.0)
  (gl:begin :points)
  (%gl:color-4f 1.0 0.0 0.0 0.5)
  (%gl:vertex-3f (sfloat *mouse-x*) (sfloat *mouse-y*) 0.0f0)
  (gl:end))

(defun perspective ()
  (change-mode :projection)
  (glu:perspective 45.0f0 (/ (get_w) (get_h)) 0.1f0 100000.0f0)
  (change-mode :modelview))

(defparameter *background-color* +black+)

(defun set-background-color (color)
  (setf *background-color* color)
  (clean-up-forms
    (setf *background-color* +black+)))  

(defun clear-to-background ()
  (gl:clear-color
   (color-r *background-color*)
   (color-g *background-color*)
   (color-b *background-color*)
   (color-a *background-color*)))

(cffi:defcfun ("swap_buffers" swap_buffers) :void)

(defun draw-scene ()  
  (perspective)
  (gl:load-identity)
  (let ((pan (transformation-translation *camera*)))
    (%gl:translate-d (point-x pan) (- (point-y pan)) (point-z pan)))

  (gl:enable :lighting)
  (setup-light)
 
  (%gl:rotate-f (point-y (transformation-rotation *camera*)) 1.0f0 0.0f0 0.0f0)
  (%gl:rotate-f (point-x (transformation-rotation *camera*)) 0.0f0 1.0f0 0.0f0)

  (clear-to-background)
  (gl:clear :color-buffer-bit)
  (gl:clear :depth-buffer-bit)
  (funcall *draw-command*)

  (gl:disable :lighting)
  (opengl-draw-mouse)

  (swap_buffers))

(defparameter *key-map* nil)

(defun init-key-map () 
  (setf *key-map* (make-array 512 :initial-element nil)))

(defun key (key-name)
  (elt *key-map* key-name))

(defun set-key (key-name state)
  (setf (elt *key-map* key-name) state)
  0) ; do not return

(defconstant +key-shift+ 304)
(defconstant +key-ctrl+ 306)
(defconstant +key-x+ 120)
(defconstant +key-y+ 121)
(defconstant +key-z+ 122)

(defun enable-antialiasing ()
  (gl:enable :polygon-smooth)
  (gl:hint :polygon-smooth-hint :nicest)
  (gl:enable :line-smooth)
  (gl:hint :line-smooth-hint :nicest)
  (gl:enable :point-smooth)
  (gl:hint :point-smooth-hint :nicest))

(defun init-gl-settings ()
  (gl:clear-depth 1.0f0)  
  (gl:shade-model :smooth)
  (gl:enable :color-material)
  (gl:enable :light0)    
  (gl:enable :blend)
  (gl:enable :alpha-test)
  (gl:alpha-func :greater 0.1)
  (gl:enable :normalize)
  (gl:enable :texture-2d)
  (%gl:light-model-i :light-model-two-side 1)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (if *anti-aliasing* (enable-antialiasing))
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:hint :perspective-correction-hint :nicest))

(cffi:defcfun ("save_screen" save_screen) :void
  (name :string))

(defun write-pnm-p3-file (file-name)
  (save_screen (get-full-path file-name))
  (format t "done writing ~A~%" file-name))

(cffi:defcfun ("exit_graphics" exit_graphics) :void)

(defun bail-out ()
  (delete-textures)
  (free-all-display-lists)
  (exit_graphics))

(defun get-move-domain ()
  (if (or (key +key-ctrl+) (key +key-shift+))
      (transformation-translation *camera*)
      (transformation-rotation *camera*)))

(defun move-modifier (dx dy dz)
  (if (key +key-ctrl+)
      (create-point 0 0 dz)
      (create-point dx dy 0)))

(defun mouse-move (x y dx dy dz state)
  (setf *mouse-x* x)
  (setf *mouse-y* y)
  (if (not (= 0 state))
      (point-addf (get-move-domain) (move-modifier dx dy dz))))

(defun write-pnm-file ()
  (write-pnm-p3-file (indexed-name "result" ".pnm")))

(defun keyboard (key)
  (let ((result 0))
    (cond ((or (= key 027) (= key 113)) (setf result 1))
	  ((= key 115) (write-pnm-file))
	  ((= key 114) (povray:save-povray))
	  ((= key 119) (wavefront-save))
	  ((= key 106) (json:save-json))
	  ((= key 98) (rib:save-rib))
	  ((= key 99) (collada:save-collada)))
    (set-key key t)
    result))

(cffi:defcfun ("emit_char" emit_char) :void
  (num :int))

(cffi:defcfun ("event_loop" event_loop) :void  
  (draw-fn :pointer)
  (mouse-fn :pointer)
  (key-fn :pointer))
   
(cffi:defcallback draw_scene :void ()
  (draw-scene))

(cffi:defcallback key_change :int ((pressed? :int) (key :int))
  (if (= 0 pressed?) (set-key key nil) (keyboard key)))

(cffi:defcallback mouse_move :void 
    ((x :int) (y :int) (dx :int) (dy :int) (state :int))
  (mouse-move x y dx dy dy state))

(defun simple-event-loop ()
  (event_loop 
   (cffi:callback draw_scene)
   (cffi:callback key_change)
   (cffi:callback mouse_move)))

(cffi:defcfun ("init_graphics" init_graphics) :void)

(defvar *disable-opengl* nil)

(defun start-opengl ()
  (clean-up-forms
    (bail-out))
  (init_graphics)
  (init-key-map)
  (init-gl-settings)
  (load-textures)
  (update-commands)
  (simple-event-loop))

(defun run-opengl ()
  (if *disable-opengl*
      (funcall *disable-opengl*)
      (start-opengl)))

(cffi:defcfun ("load_picture" load_picture) :pointer
  (name :string))

(cffi:defcfun ("delete_picture" delete_picture) :void
  (ptr :pointer))

(cffi:defcfun ("get_integrated" get_integrated) :float
  (ptr :pointer)
  (x :float)
  (y :float)
  (c :int)
  (s :float))

(defun get-integrated-color (ptr x y s)
  (create-color
   (get_integrated ptr x y 0 s)
   (get_integrated ptr x y 1 s)
   (get_integrated ptr x y 2 s)
   (get_integrated ptr x y 3 s)))

(defmacro with-picture (var file-name &body body)
  `(let ((,var (load_picture ,file-name)))
     ,@body
     (delete_picture ,var)))

(defun test ()
  (with-picture pic "rose-leaf.png"
    (format t "~A~%" pic)))
		
