;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:abop)

(defconstant +fibonacci+ (* 360 (expt (/ 2 (+ 1 (sqrt 5))) 2)))

(defparameter *green* (create-color 0.0 0.6 0.0))
(defun green () (color *green*))

(defun gosper ()
  (with-graphics
    (iteration-count 4)
    (set-angle 60)
    (add-option Fr (turtle-forward 10))
    (add-option Fl (turtle-forward 10))
    (set-axiom (radius(5) -(110) Fl))
    (add-rule (Fl -> Fl + Fr + + Fr - Fl - - Fl Fl - Fr +))
    (add-rule (Fr -> - Fl + Fr Fr + + Fr + Fl - - Fl - Fr))))

(defun figure-1.39 ()
  (with-graphics
    (iteration-count 10)
    (set-angle 85)
    (add-option F (turtle-forward x))
    (set-axiom (radius(5) A))
    (add-rule (A -> F(10) [ + A ] [ - A ]))
    (add-rule (F(x) -> F((* 1.456 x))))))

(defun figure-1.31-d ()
  (with-graphics
    (iteration-count 26)
    (set-axiom (F O F I F I))
    (add-option +r (turtle-turn +25.75))
    (add-option -r (turtle-turn -25.75))
    (add-rule (:nameless F -> forward(5)))
    (add-rule (:nameless +r -> -r))
    (add-rule (:nameless -r -> +r))
    (add-rule ((O < O > O -> I)
	       (O < O > I -> O)
	       (I < O > O -> I)
	       (I < O > I -> I [ +r F I F I ])
	       (* < O > * -> O)))
    (add-rule ((O < I > O -> O)
	       (I < I > O -> I)
	       (I < I > I -> O)
	       (O < I > I -> I F I)
	       (* < I > * -> I)))))

(defparameter *twig-color* (create-color 0.4 0.33 0.1))

(defun figure-1.25 ()
  (with-graphics
    (iteration-count 14)
    (set-axiom (color(*twig-color*) radius(2.2) A(2)))
    (add-macro (leaf-bit(x) -> dot edge(x) turn(22) edge(x) turn(22) edge(x)))
    (add-rule (S -> SS))
    (add-rule (F -> FF))
    (add-rule (SS -> F L))
    (add-rule (FF -> forward(12) F roll(-112.5) S))
    (add-rule (B(x) -> [ pitch(22.5) F radius(x) L A((* x 0.8)) ]))
    (add-rule (A(x) -> B(x) roll(-112.5) B(x) roll(-157.5) B(x)))
    (add-rule (L -> [ pitch(45) green
		    [ roll(-10) { turn(-22) leaf-bit(+15) } ] jump(42) 
		    [ roll(+10) { turn(-22) leaf-bit(-15) } ] ]))))

(defparameter *pink* (create-color 1.0 0.7 0.7))
(defparameter *yellow* (create-color 1.0 1.0 0.0))

(defun figure-1.26 ()
  (with-graphics
    (iteration-count 5)
    (set-angle 18)
    (add-option F (turtle-forward 14))
    (add-option M (turtle-polygon-forward 14))
    (set-axiom (green plant))
    (add-rule (F -> F))
    (add-rule (M -> M))
    (add-macro (seg -> r-seg((random 99))))
    (add-macro (sub-fragment(x) -> [ roll(-36) pitch(x) leaf ]))
    (add-macro (fragment -> sub-fragment(+36) sub-fragment(-36) F seg))
    (add-rule (internode -> F seg fragment))
    (add-rule (pedicel -> color(*pink*) F F))
    (add-macro (LM -> - M + M M + M))
    (add-rule (leaf -> [ green { dot LM - -(180) LM } ]))
    (add-macro (RW -> roll(72) wedge))
    (add-rule (flower -> [ roll(54) pedicel roll(18) wedge RW RW RW RW ]))
    (add-rule (wedge -> [ color(*yellow*) pitch(-18) F ]
		        [ { dot pitch(72) - M + M +(180) - M + M } ]))
    (add-rule (plant -> internode + [ plant + flower ] - - roll(-36)
	                [ - - leaf ] internode [ + + leaf ] -
	                [ plant flower ] + + plant flower))
    (add-rule ((r-seg (x) ? (< x 33) -> seg)
	       (r-seg (x) ? (< x 66) -> seg F seg)
	       (r-seg (x) ? (< x 99) -> seg fragment)))))

(defun figure-2.6-d ()
  (with-graphics
    (iteration-count 10)
    (set-axiom (radius(12) A(100 10)))
    (add-macro (Q -> 0.707))
    (add-macro (D -> -137.5))
    (add-macro (X(rule turn) -> forward(l) radius(w) [ turn(-30) flip
                                rule((* 0.7 l) (* Q w)) ]
                                rule((* 0.9 l) (* Q w))))
    (add-rule (A(l w) -> forward(l) radius(w) 
                         [ pitch(30) B((* 0.7 l) (* Q w)) ]
                         roll(d) A((* 0.9 l) (* Q w))))
    (add-rule (B(l w) -> X(C -)))
    (add-rule (C(l w) -> X(B +)))))

(defun figure-2.7-d ()
  (with-graphics
    (iteration-count 10)
    (set-axiom (roll(90) radius(12) A(100 10)))
    (add-macro (Q -> 0.707))
    (add-rule (A(l w) -> forward(l) radius(w)
                         [ pitch(35) B((* 0.9 l) (* Q w)) ] roll(-180)
                         [ pitch(35) B((* 0.8 l) (* Q w)) ]))
    (add-rule (B(l w) -> forward(l) radius(w)
		         [ +(35) flip B((* 0.9 l) (* Q w)) ]
		         [ -(35) flip B((* 0.8 l) (* Q w)) ]))))

(defparameter *gravity* (create-point -0.00 -1.0 0.0))

(defun figure-2.8-c ()
  (with-graphics
    (iteration-count 8)
    (init-segments :count 12)
    (add-option thick (turtle-set-radius (* 0.2 w)))
    (add-option F (turtle-tropism l *gravity* 0.23))
    (set-axiom (thick(1.3) F(80) roll(-45) A))
    (add-macro (branch -> [ pitch(22.50) F(20) A ]))
    (add-macro (vr -> 1.732))
    (add-rule (F(l) -> F((* 1.079 l))))
    (add-rule (thick(w) -> thick((* vr w))))
    (add-rule (A -> thick(vr) F(20) thick(vr)
		    branch roll(-112.50) branch roll(-157.50) branch))))
	
(defparameter *fruit-color* (create-color 0.3 0.4 0.0))

(defun figure-3.5 ()
  (with-graphics
    (iteration-count 35)
    (read-wavefront "purse-flower.obj")
    (read-wavefront "purse-fruit.obj")
    (read-wavefront "purse-leaf.obj")
    (set-length 2)
    (add-option C (turtle-object "flower" 3.0 +white+))
    (set-axiom (green radius(1.0) I(9) A(13)))
    (add-macro (A-macro -> [ pitch(70) L ] roll(-137.5) I(10)))
    (add-rule ((I(x) ? (> x 0) -> F I((1- x)))
	       (I(x) ? (= x 0) -> F)))
    (add-rule ((A(x) ? (> x 0) -> A-macro A((1- x)))
	       (A(x) ? (= x 0) -> A-macro B)))
    (add-rule ((u(x) ? (> x 0) -> pitch(9) u((1- x)))
	       (u(x) ? (= x 0) -> pitch(9))))
    (add-rule ((C(x) ? (> x 0) -> C((1- x)))
	       (C(x) ? (= x 0) -> C(-1) D)))
    (add-rule (B -> [ pitch(18) u(4) F I(10) I(5) C(9) ] roll(137.5) I(8) B))
    (add-rule (D -> pitch(-30) obj("fruit" 15.0 *fruit-color*)))
    (add-rule (L -> obj("leaf" 8.0 *green*)))))

(defparameter *lilac* (create-color 0.5 0.25 0.5))

(defun figure-3.19 ()
  (with-graphics
    (iteration-count 19)
    (read-wavefront "lilac.obj")
    (add-option F (turtle-tropism x *gravity* 0.02))
    (set-axiom (green radius(2) forward(50) A K))    
    (add-macro (R(x) -> [ +(x) roll(-60) K ]))
    (add-rule (A -> R(-45) R(+45) I(0) roll(-90) A))
    (add-macro (Z(x) -> [ +(x) F(15) A ]))
    (add-rule ((I(x) ? (= x 4) -> I((1+ x)) Z(-45) Z(+45))
               (I(x) -> F(3) I((1+ x)))))
    (add-rule (K -> [ roll((- 10 (random 20))) obj("lilac" 30.0 *lilac*) ]))
    (add-rule (F(x) -> F(x)))))

(defun figure-4.1 ()
  (with-graphics
    (iteration-count 1000)
    (read-wavefront "sphere.obj")
    (set-axiom (jump(300) A(0)))
    (add-rule (D -> obj("lode" 15.0 *green*)))
    (add-rule (A(x) -> +(+fibonacci+) [ jump((* 12 (sqrt x))) D ] A((1+ x))))))

(defun figure-4.11 ()
  (with-graphics
    (iteration-count 205)
    (read-wavefront "sphere.obj")
    (set-axiom (A(0)))
    (add-rule (D -> obj("lode" 25.0 +red+)))
    (add-rule (A(x) -> [ pitch(90) jump(45) pitch(90) turn(180) D ]
	               jump(3) roll(+fibonacci+) A((1+ x))))))

(defun figure-5.8 ()
  (with-graphics
    (iteration-count 25)
    (add-option G2 (turtle-jump (* 0.6 s)))
    (add-option G3 (turtle-jump (* 0.6 s)))
    (set-axiom (green pitch(-15) [ { A(0 #'+) dot } ] [ { A(0 #'-) dot } ]))
    (add-rule (A(x f) -> dot +(2) G2(5.0 1.15) dot [ +((funcall f 60))
		         B(x) G3(3.0 1.19 x) dot } { dot ] dot 
			 [ +((funcall f 60)) B(x) dot } { dot ] 
			 A((1+ x) f)))
    (add-rule ((B(x) ? (> x 0) -> G2(1.3 1.25) B((1- x)))
	       (B(x) -> B(x))))
    (add-rule (G2(s r) -> G2((* s r) r)))
    (add-rule ((G3(s r x) ? (> x 1) -> G3((* s r) r (1- x)))
	       (G3(s r x) -> G3(s r x))))))

(defun monkey-tree ()
  (with-graphics
    (iteration-count 30)
    (read-wavefront "monkey.obj")
    (add-option thick (turtle-set-radius x))
    (add-option head (turtle-object "suzanne" x +red+))
    (set-axiom (green apex))
    (add-rule (apex -> thick(1) f(20) roll(+fibonacci+) branch apex))
    (add-rule (thick(x) -> thick((1+ x))))
    (add-rule (head(x) -> head((* 1.03 x))))
    (add-rule (branch -> [ bend head(20.0) ]))
    (add-rule (bend -> pitch(4.0) thick(1) forward(20) bend))))

(defun bacopa ()
  (with-graphics
    (iteration-count 15)
    (read-wavefront "bacopa-leaf.obj")
    (set-length 2.0)
    (add-option P (turtle-pitch x))
    (add-option L (turtle-object "leaf" x *green*))
    (add-option F (turtle-tropism x *gravity* -0.03))
    (set-axiom (radius(2) green init(0) init(2) init(1) init(3) init(1)))
    (add-macro (b-size -> 10))
    (add-macro (single-leaf -> [ P(-20) L(1) ]))
    (add-macro (init(x) -> [ pitch(-30) roll((random 180)) D(x) ] roll(72)))
    (add-rule ((D(x) ? (> x 0) -> D((1- x)))
	       (D(x) ? (= x 0) -> A(0))))
    (add-rule (P(x) -> P((1+ x))))
    (add-rule (A(x) -> B(b-size) leaf roll(90) Z(x) A((1+ x))))
    (add-rule ((B(x) ? (> x 0) -> F((* 2 (- b-size x))) B((1- x)))))
    (add-rule (leaf -> single-leaf roll(180) single-leaf))
    (add-rule ((Z(x) ? (= x 2) -> [ roll((random 180)) pitch(30) A(0) ])
	       (Z(x) -> Z(x))))
    (add-rule ((L(x) ? (< x 10) -> L((+ x 1.5)))
	       (L(x) -> L(x))))
    (add-rule (F(x) -> F(x)))))

(defun negative-random (amount)
  (- amount (random (* 2 amount))))

(defun displacement (amount)
  (create-point (negative-random amount) 0 (negative-random amount)))

(defun patch-of-grass ()
  (with-graphics    
    (iteration-count 20)
    (init-segments :count 3)
    (add-option F (turtle-tropism 10 *gravity* q))
    (add-macro (direction -> (random 360)))
    (add-macro (tropism-ratio -> (+ 0.1 (* 0.2 (unit-random)))))
    (add-macro (displace -> move((displacement 300))))
    (set-axiom (green 500 tuft))
    (add-rule (F(q) -> F(q)))
    (add-rule (tuft -> displace roll(direction) 3 blade))
    (add-rule (blade -> roll(120) [ turn(10) apex(15 tropism-ratio) ]))
    (add-rule (apex(x q) ? (> x 0) -> radius((* 0.25 x))
	                              F(q) apex((1- x) q)))))

(defun saknes-resnums (x &optional (max 2)) (+ max (/ max (- x))))

(defun anubias ()
  (with-graphics
    (iteration-count 310)
    (init-segments :count 16)
    (assign-texture "branching" "rizatoma.png")
    (read-wavefront "anubias-leaf.obj" "anubias.png")
    (add-option resner (turtle-set-radius y))
    (add-option lapa (turtle-object "anubias-leaf" (min (+ 13 (random 5)) x)))
    (add-option TF (turtle-tropism x *gravity* y))
    (add-macro (thifo(x y) -> radius(x) F(y)))
    (add-macro (zarvar -> (/ +fibonacci+ 3)))
    (set-axiom (pitch(90) thifo(0 3) thifo(10 6) rizatoma(0 0)))
    (add-rule (TF(x y) -> TF(x y)))
    (add-rule (resner(x y) -> resner(x (min x (1+ y)))))
    (add-rule (zars(i resnums garums angle) ? (> i 0) ->
		resner(resnums 1) F(garums) pitch(angle)
		zars((1- i) (* 0.9 resnums) (* 1.025 garums) (* 0.4 angle))))
    (add-rule (lapa(x)  -> lapa((1+ x))))
    (add-rule (subsakne(r) ? (and (> r 1.5) (= 0 (random 3)))  ->
		[ roll((random 360)) pitch(-30)
		thifo(1.2 (+ 15 (random 10))) thifo(0.4 1) thifo(0 1) ]))
    (add-rule (sakne(i) ? (> i 0) -> 
		    radius((saknes-resnums i)) roll((random 30)) turn(5) 
		    TF(5 0.1) subsakne((saknes-resnums i)) sakne((1- i))))
    (add-rule ((rizatoma(x y) ? (> x 1) -> rizatoma((1- x) y))
	       (rizatoma(x y) ? (= x 1) -> thifo(15 10) rizatoma(0 y))
	       (rizatoma(x y) ? (= x 0) ->
		    turn((+ -30 (* 60 (unit-random))))
		    [ roll((- (mod (* y zarvar) 120) 60)) jump(10)
		      [ zars(20 15 5 (+ -75 (* 3.5 y)))
                        flip roll(180) pitch(80) lapa(0) ]
		      [ flip pitch(-90) sakne((+ 10 (random 30))) ] ]
		    rizatoma(30 (1+ y)))))))

(defparameter *skujas-kraasa* (create-color 0.15 0.3 0.0))
(defparameter *zaru-kraasa* (create-color 0.37 0.28 0.13))
(defparameter *biezums* 0.5)

(defun egle ()
  (with-graphics
    (iteration-count 7)
    (read-wavefront "skuja.obj")
    (add-option TF (turtle-tropism x *gravity* 0.7))
    (add-option skuja (turtle-object "skuja" 0.5 *skujas-kraasa*))
    (add-macro (skujas(x y) -> [ y skujo(x) ] radius(x) TF((* y *biezums*))))
    (set-axiom (color(*zaru-kraasa*) stumbrs(4)))
    (add-rule (skuja -> skuja))
    (add-rule (TF(x) -> TF(x)))
    (add-rule (skujo(x) -> [ turn(60) jump(x) skuja ] 
	       jump(*biezums*) roll(+fibonacci+)))
    (add-rule (apzaro(i) ? (> i 0) -> flip pitch(15) skujas(1 100)
		     [ turn(+45) apzaro((1- i)) ]
		     [ turn(-45) apzaro((1- i)) ] 
		     apzaro((1- i))))
    (add-rule (stumbra-apzarojums(i) -> roll(120) [ pitch(45) apzaro(i) ]))
    (add-rule (stumbrs(i) ? (> i 0) ->
		      roll(+fibonacci+) 3 stumbra-apzarojums(i)
		      skujas((* 1.2 i) 200) stumbrs((1- i))))))

(defparameter *rose-green* (create-color 0.25 0.35 0.10))

(defun rose ()
  (with-graphics
    (iteration-count 45)
    (read-wavefront "rose-sting.obj")
    (read-wavefront "rose-petal.obj" "petal.png")
    (read-wavefront "rose-leaf.obj" "rose-leaf.png")
    (read-wavefront "rose-sub.obj" "rose-sub.png")
    (add-option G (turtle-tropism x *gravity* 0.4))
    (set-axiom (stem(6) sub(6) radius(3.5) F(5) radius(20) flower(45)))
    (add-rule (G(x) -> G(x)))
    (add-rule ((branch(x o) ? (or (= x 3) (= x 4)) -> [ roll((* x 45))
		      pitch((* o 45)) radius(1) twig(2) G(20) leaf(3) ])
	       (branch(x o) ? (= o -1) ->
		      [ jump(-10) obj("sting" 7 *rose-green*) ])))
    (add-rule (stem(x) ? (> x 0) -> color(*rose-green*) roll(+fibonacci+)
	turn(5) radius(3) F(100) branch(x +1) branch(x -1) stem((1- x))))
    (add-rule (twig(x) ? (> x 0) -> twig((1- x)) G(50)
        [ turn(+80) G(10) leaf(x) ] [ turn(-80) G(10) leaf(x) ]))
    (add-rule (leaf(x) -> flip pitch(10)
	[ obj("leaf" (+ 12 (* 6 x))) ]))
    (add-rule (sub(x) ? (> x 0) -> roll(60)
        [ turn((random 5)) obj("sub" 15) ] sub((1- x))))
    (add-rule (flower(x) ? (> x 0) -> roll(+fibonacci+)
	jump (1.2) petal(x) flower((1- x))))
    (add-rule (petal(x) -> [ turn((* -0.8 45 (expt (/ x 45) 4)))
	obj("petal" (* 0.5 x))]))))

(defun birch-tropism (s x)
  (turtle-set-radius s)
  (turtle-tropism x *gravity* (/ 0.3 s)))

(defun birch-texture-fade (x)
  (expt x 9))

(defun birch-uv-mapping (radius vertical)
  (declare (ignore vertical))
  (- 0.99 (birch-texture-fade (* 0.2 (- radius 0.2)))))

(defun birch ()
  (with-graphics
    (iteration-count 20)
    (set-uv-calculator #'birch-uv-mapping)
    (set-background-color (create-color 0.0 0.2 0.4))
    (assign-texture "branching" "birch-bark.png")
    (read-wavefront "leaf.obj" "birch-leaf.png")
    (add-option leaf (turtle-object "leaf" 5.0))
    (add-option wood (birch-tropism s x))
    (add-option lumber (progn (turtle-set-radius s) (turtle-forward x)))
    (add-option bend (turtle-pitch x))
    (set-axiom (lumber(.3 6) trunk))
    (add-rule (lumber(s x) -> lumber((* 1.15 s) (* 1.2 x))))
    (add-rule (bend(x) -> bend((min 90 (+ x 2)))))
    (add-rule (trunk -> lumber(.3 2) roll(+fibonacci+) [ bend(0) twig ] trunk))
    (add-rule (wood(s x) -> wood((* 1.15 s) x)))
    (add-rule ((leaf(x) ? (< x -10) -> )
	       (leaf(x) ? (/= x 0) -> leaf((1- x)))
	       (leaf(x) ? (= x 0) -> leaf(-1) twig)))
    (add-macro (offshoot ->  [ bend(20) flip leaf(2) ]))
    (add-macro (leaves -> [ bend(20) leaf(-1) ] roll(+fibonacci+) jump(20/7)))
    (add-rule (twig -> [ leaves leaves leaves leaves leaves leaves leaves ]
		    wood(.2 20) turn(10) offshoot roll(+fibonacci+) twig))))

(defparameter *fikuss-color* (create-color 0.21 0.67 0.062))

(defun fikuss ()
  (with-graphics
    (iteration-count 20)
    (init-segments :count 8)
    (set-axiom (color(*fikuss-color*) obj("clay-pot" 40.0) jump(100) A))
    (add-rule (A -> W(1) S(10) [ pitch(90) L(5) ] roll(+fibonacci+) A))
    (add-rule (S(x) -> S((* 1.09 x))))
    (add-rule (W(x) -> W((* 1.07 x))))
    (add-rule (L(x) -> L((* 1.05 x))))
    (add-option W (turtle-set-radius x))
    (add-option S (turtle-forward x))
    (add-option L (turtle-object "leaf" x))
    (read-wavefront "fikuss.obj" "fikuss.png")
    (read-wavefront "clay-pot.obj" "clay-pot.png")))
  
(defparameter *barley-yellow* (create-color 0.847 0.698 0.176))

(defun barley ()
  (with-graphics
    (iteration-count 25)
    (set-axiom (color(*barley-yellow*) radius(3) forward(100) A(20 13)))
    (add-rule (A(x y) -> [ turn(x) jump(-5) G ] jump(y)
		A((- (* 0.98 x)) (* 1.01 y))))
    (add-rule (G -> radius(0) F(15) radius(5) F(15) radius(6) F(15) radius(5)
		 F(15) radius(2) F(15) flip pitch(7) radius(1) 10 F(15)))))

(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun maple-length-mod (x)
  (* 35.0 (sigmoid (* 0.25 (- x (* 1/6 *total-iterations*))))))
 
(defparameter *maple-max-size* 0)

(defun maple-radius-mod (x)
  (+ 0.2 (* 9.8 (expt (/ x *maple-max-size*) 4.0))))

(defvar *branch-count* nil)
(defvar *leaf-count* nil)

(defun maple-brach (s x c)
  (when (not (eq c 'dead))
    (let ((width (maple-radius-mod s)))
      (incf *branch-count*)
      (turtle-set-radius width)
      (turtle-tropism (maple-length-mod x) *gravity* (/ -0.05 width))
      (when (< s 3)
	(turtle-push)
	(turtle-flip-up)
	(incf *leaf-count*)
	(turtle-roll (random 360))
	(turtle-pitch -120)
	(turtle-object "leaf" 7.0)
	(turtle-pop)))))

(defun maple-trunk (x)
  (setf *maple-max-size* x)
  (turtle-set-radius (maple-radius-mod x)))

(defun strahler (&rest args)
  (let ((maximum (apply #'max args)))
    (if (> (count maximum args) 1)
	(1+ maximum)
	maximum)))

(defun chance (odds)
  (> odds (random 100)))

(defun branch? (type odds)
  (and (< *iterations-so-far* (/ *total-iterations* 2))
       (member type '(new apex))
       (chance odds)))
		   
(defun maple-system ()
  (with-graphics
    (init-segments :count 6)
    (iteration-count 50)
    (read-wavefront "leaf.obj" "maple-leaf.png")
    (assign-texture "branching" "bark.png")
    (add-option wood (maple-brach s x c))
    (add-option trunk (maple-trunk x))
    (set-axiom (trunk(0) F(100) wood(1 1 'apex)))
    (add-rule (leaf -> leaf))
    (add-rule (trunk(x) > wood(s x c) -> trunk((1+ s))))
    (add-rule ((wood(s x c) ? (and (numberp c) (> c 0)) -> wood(s x (1- c)))
	       (wood(s x c) ? (and (numberp c) (= c 0)) -> wood(s x 'new))
	       (wood(s x c) ? (and (chance 10) (eq c 'apex)) -> wood(s x c))
	       (wood(s x c) ? (branch? c 95) -> 
		    wood(s x 'old) roll(+fibonacci+)
		    [ pitch((random 5)) wood(1 1 c) ] roll(180)
		    [ pitch((+ 30 (random 10))) wood(1 1 1) ])
	       (wood(s x c) ? (branch? c 50) ->
		    wood(1 1 (random 3)))	       
	       (wood(s x c) > [ wood(s1 x1 c1) * ]
		              [ wood(s2 x2 c2) * ] ->
		    wood((max s (strahler s1 s2)) (1+ (max x1 x2)) c))
	       (wood(s x c) -> wood(s x c))))))

(defun maple ()
  (let ((*branch-count* 0)
	(*leaf-count* 0))
    (maple-system)
    (format t "branch-count=~A~%" *branch-count*)
    (format t "leaf-count=~A~%" *leaf-count*)))

(defun chestnut-radius-mod (i)
  (* 0.25 (expt 1.05 (- *total-iterations* i))))

(defun chestnut-length-mod (i)
  (* 10.0 (expt 1.02 (- *total-iterations* i))))

(defun chestnut-tropism-mod (i)
  (* 1.5 (- (+ 0.35 (* 0.01 (crandom 11))) (/ i *total-iterations*))))

(defun chestnut-brach (i)
  (turtle-set-radius (chestnut-radius-mod i))
  (turtle-tropism (chestnut-length-mod i) *gravity* (chestnut-tropism-mod i))
  (when (> i (- *total-iterations* 10))
    (dotimes (i 6)
      (turtle-push)
      (turtle-pitch 30)
      (turtle-object "chestnut" 4.0)
      (turtle-pop)
      (turtle-roll 60))))

(defun chestnut ()
  (with-graphics
    (iteration-count 100)
    (init-segments :count 10)
    (set-axiom (wood(-5) A(0 100)))
    (read-wavefront "chestnut.obj" "chestnut.png")
    (assign-texture "branching" "bark.png")
    (add-option wood (chestnut-brach i))
    (add-rule (wood(i) -> wood(i)))
    (add-option chestnut-turn (turtle-turn x))
    (add-rule (chestnut-turn(x) -> chestnut-turn((- x 0.2))))
    (add-rule 
     ((A(x p) ? (and (< p 10) (> *iterations-so-far* 20)) -> ) ; die
      (A(x p) ? (> x 0) -> A((1- x) 100))
      (A(x p) -> wood(*iterations-so-far*)
                 [ chestnut-turn(60) flip A(10 (crandom 100)) ] 
	         turn(-20) roll(+fibonacci+) A(5 100))))))

(defun dead-tree-branch (x)
  (turtle-tropism (* 15.0 (expt 1.07 (sqrt x))) *gravity* 0.1))

(defun dead-random ()
  (+ 0.2 (* 0.6 (unit-random))))

(defun dead-tree ()
  (with-graphics
    (iteration-count 50)
    (init-segments :count 8)
    (assign-texture "branching" "bark.png")
    (add-option branch (dead-tree-branch x))
    (add-rule (branch(x) -> branch(x)))
    (set-axiom (roll((- +fibonacci+)) A(1000.0 (dead-random))))
    (add-rule ((A(s r) ? (< s 1.0) -> )
	       (A(s r) -> 
		 surface(s) branch(s) roll(+fibonacci+)
		 [ turn((* 60 r)) A((* s (- 1.0 r)) (dead-random)) ] roll(180)
		 [ turn((* 60 (- 1.0 r))) A((* s r) (dead-random)) ])))))

(defun oak-branch-length(x)
  (+ 1.0 (* 60.0 (sigmoid (* 0.2 (- x (* 1/2 *total-iterations*)))))))

(defun oak-branch (x)
  (turtle-set-surface (* 0.2 (expt 1.1 (expt x 1.3))))
  (turtle-forward (oak-branch-length x))
  (when (= 1 x) (turtle-object "bouquet" 5.0)))

(defun oak ()
  (with-graphics
    (iteration-count 35)
    (set-axiom (B(3) B(2) B(1) [ turn(+30) A ] [ turn(-30) A ] ))
    (init-segments :count 8)
    (read-wavefront "bouquet.obj" "oak-leaf.png")
    (assign-texture "branching" "bark.png")
    (add-option B (oak-branch x))
    (add-rule (B(x) -> B((1+ x))))
    (add-rule ((D(x) ? (= 0 (random 2)) -> D((1+ x)))
	       (D(x) -> A rip)))
    (add-rule (A -> B(1) roll(+fibonacci+)
		[ pitch(+30) D(1) A ]
		[ pitch(-30) D(1) A ]))))

(defconstant +golden-ratio+ 1.61803399)
(defparameter *spruce-angle* (/ 360.0 8.0))
(defparameter *spruce-variation* (/ *spruce-angle* +golden-ratio+))
(defparameter *spruce-needle* (create-color 0.15 0.3 0.0))
(defparameter *spruce-wood* (create-color 0.37 0.28 0.13))

(defun spurce-needles (x)
  (turtle-jump -4.0)
  (turtle-object "skuja" 0.07 *spruce-needle*)
  (when (> x 4.0)
    (spurce-needles (- x 4.0))))

(defun spruce-random ()
  (- (* 2.0 (unit-random)) 1.0))

(defun spruce-wood (x a)
  (when (/= a 0.0) 
    (turtle-pitch (+ a (spruce-random))))
  (turtle-forward x)
  (turtle-push)
  (spurce-needles x)
  (turtle-pop))

(defun needles (&optional (iterations 45))
  (with-output-to-file
    (set-axiom (apex))
    (import-vars iterations)
    (iteration-count iterations)
    (read-wavefront "skuja.obj")
    (set-output-file "needles.obj")
    (add-rule (skuja -> skuja))
    (add-rule (bend(x) -> bend((+ (/ 45.0 iterations) x))))
    (add-option bend (turtle-pitch x))
    (add-rule (apex -> [ bend(45) skuja ] roll(+fibonacci+) jump(1.4) apex))
    (add-option skuja (turtle-object "skuja" 4.0 *spruce-needle*))))

(defvar *balls?* nil)

(defun spruce-ball (x)
  (let* ((tx (point-x (state-location *turtle*)))
	 (tz (point-z (state-location *turtle*)))
	 (dist (sqrt (+ (expt tx 2) (expt tz 2)))))
    (when (and *balls?* (= x 2) (> dist 20.0))
      (turtle-push)
      (setf (state-orientation *turtle*)
	    (make-vector-space
	     :y (copy-point +y-axis+)
	     :x (point-normalize (create-point tx 0.0 tz))
	     :z (point-normalize (create-point tz 0.0 (- tx)))))
      (turtle-roll 90)
      (let ((size (* 1.3 (expt dist 4/10))))
	(turtle-jump (- size))
	(turtle-object "suzanne" size +red+))
      (turtle-pop))))

(defun spruce ()
  (needles)
  (with-graphics
    (iteration-count 20)
    (read-wavefront "needles.obj")
    (read-wavefront "monkey.obj")
    (set-axiom (jump(50) color(*spruce-wood*) thickness(0.5) apex))

    (add-option bend (turtle-pitch x))
    (add-option thickness (turtle-set-surface x))
    (add-option wood (spruce-wood x a))
    (add-option ball (spruce-ball x))

    (add-macro (B(x) -> roll(*spruce-angle*) [ bend(x 4.0) branch-tip(0) ] ))
    (add-macro (alternate(x fn) -> funcall((if (oddp x) fn #'noop))))
    (add-macro (M -> obj("suzanne" 10.0 +red+)))

    (add-rule (noop -> noop))
    (add-rule (wood(x a) -> wood(x a)))
    (add-rule (ball(x) -> ball((1+ x))))
    (add-rule (bend(x val) -> bend((+ x val) val)))    
    (add-rule (sub-twig -> [ flip +(45) wood(8 5) ] [ flip -(45) wood(8 5) ]))
    (add-rule (twig(x) -> alternate(x #'sub-twig) wood(4 5) twig((1+ x))))
    (add-rule (thickness(x) -> thickness((* 1.25 x))))
    (add-rule (init-twigs ->
		[ +(45) flip radius(0.5) pitch((- 15 (random 60))) twig(0) ]
		[ -(45) flip radius(0.5) pitch((- 15 (random 60))) twig(0) ]))
    (add-rule (delayed-branch-segment -> wood(12.0 -4.0)))
    (add-rule (branch-tip(x) -> thickness(0.5) ball(0) delayed-branch-segment
			 alternate(x #'init-twigs) branch-tip((1+ x))))
    (add-rule (apex -> thickness(0.5) roll(*spruce-variation*) wood(24 0) 
		    [ B(45) B(40) B(45) B(40) B(45) B(40) B(45) B(40) ] apex))))

(defun spruce-with-balls ()
  (let ((*balls?* t))
    (spruce)))

(defparameter *pistil-color* (create-color 0.99 0.66 0.00))
(defparameter *dandelion-color* (create-color 0.32 0.37 0.21))
(defparameter *dandelion-tropism* (create-point -0.1 1.0 0.0))

(defun dandelion ()
  (with-graphics
    (iteration-count 500)
    (set-axiom (leaves stem septals flower))

    (read-wavefront "dandelion-pistil.obj")
    (read-wavefront "dandelion-cap.obj" "dandelion-petal.png")
    (read-wavefront "dandelion-petal.obj" "dandelion-petal.png")
    (read-wavefront "dandelion-septal1.obj" "dandelion-septal.png")
    (read-wavefront "dandelion-septal2.obj" "dandelion-septal.png")
    (read-wavefront "dandelion-leaf.obj" "dandelion-leaf.png")

    ;; leaves
    (add-macro (leaves -> [ L(0) ]))
    (add-rule (L(x) ? (< x 10) -> roll(+fibonacci+) 
		[ pitch((- (* 3.0 x) 30.0)) obj("leaf" (+ 5.0 (* 0.4 x))) ] 
		L((1+ x))))

    ;; stem
    (add-macro (stem -> color(*dandelion-color*) turn(20) radius(2) S(0)))
    (add-option segment (turtle-tropism x *dandelion-tropism* 0.1))
    (add-rule (S(x) ? (< x 30) -> segment(10.0) S((1+ x))))
    (add-rule (segment(x) -> segment(x)))

    ;; septals
    (add-macro (septals -> [ S2(0) S1(0) ] jump(17)))
    (add-rule (S1(x) ? (< x 34) -> obj("septal1" 25.0) 
		 roll(+fibonacci+) jump(0.05) S1((1+ x))))
    (add-rule (S2(x) ? (< x 8) -> obj("septal2" 7.0) 
		 roll(+fibonacci+) S2((1+ x))))

    ;; flower
    (add-macro (flower -> [ obj("cap" 10.0) pitch(90) A(1) ]))
    (add-macro (petal(x) -> flip obj("petal" x)))
    (add-macro (pistil(x) -> obj("pistil" x *pistil-color*)))
    (add-rule (P1(x) -> petal((+ 1.0 (* 0.02 x)))))
    (add-rule (P2(x) ? (< x 60) -> pitch(-5) pistil(1.0)))
    (add-rule ((A(x) ? (< x 110) -> turn(+fibonacci+) 
		 [ jump((sqrt x)) pitch((- x 90.0)) P1(x) P2(x) ] 
		 A((+ 0.5 x)))))))

(defparameter *lily-pistil* (create-color 0.9 0.8 0.00))

(defun water-lily ()
  (with-graphics
    (iteration-count 60)
    (set-axiom (pitch(90) pistil(1)))
    (read-wavefront "lily-petal.obj")
    (read-wavefront "lily-pistil.obj")
    (add-rule ((pistil(x) ? (< x 30) -> turn(+fibonacci+)
		 [ jump(x) pitch((+ -85 (* 15.0 (/ x 30.0))))
		 obj("pistil" (- 13.0 (* 2.0 (/ x 30.0))) *lily-pistil*) ] 
		 pistil((1+ x)))
	       (pistil(x) -> petal(1))))
    (add-rule ((petal(x) ? (< x 30) -> turn(+fibonacci+)
		 [ jump((+ 36.0 x)) pitch((+ -75 (* 75.0 (/ x 30.0))))
		 obj("petal" (+ 30.0 (* 10.0 (/ x 30.0)))) ] 
		 petal((1+ x)))))))

(defun s-sigmoid (x &optional (q 1.0))
  (sigmoid (* q (- x (* 0.5 *total-iterations*)))))

(defun pine-cone-size (x)
  (* 15.0 (expt (- 1.0 (/ x *total-iterations*)) 0.2)))

(defun pine-seed-angle (x)
  (* 90.0 (expt (/ x *total-iterations*) 0.55)))

(defun pine-seed-displacement (x)
  (* 1.0 (expt (- 1.0 (/ x *total-iterations*)) 4.0)))

(defun pine-vary (x)
  (* x (- 1.1 (* 0.2 (unit-random)))))

(defparameter *pine-cone-color* (create-color 0.5647059 0.3882353 0.25882354))

(defun pine-wood-branch (age)
  (turtle-set-surface (expt 1.6 age))
  (turtle-tropism (pine-vary (+ 5.0 (* 0.009 (expt age 3.0)))) *gravity* -0.1)
  (when (= 0 age) (turtle-object "skuja" 0.15 *spruce-needle*)))

(defun pine-wood (age)
  (if (and (= 0 age) (= 0 (random 20)))
      (turtle-object "pine-seed" 0.15 *pine-cone-color*)
      (pine-wood-branch age)))

(defun pine-uv-mapping (radius vertical)
  (let* ((val (expt (* 0.03257329 radius) 0.8)))
    (if (> val 0.5)
	(- 0.99 (* val 0.99))	
	(+ 0.5 (* 0.5 (nth-value 1 (floor vertical)))))))

(defun pine-cone ()
  (with-output-to-file
    (set-axiom (A))
    (iteration-count 90)
    (read-wavefront "pine-cone-seed.obj")
    (set-output-file "pine-cone.obj")
    (add-rule (J(x) -> J((1+ x))))
    (add-rule (seed(x) -> seed((1+ x))))
    (add-rule (bend(x) -> bend((1+ x))))
    (add-rule (A -> [ bend(0) roll(180) seed(0) ] roll(+fibonacci+) J(0) A))
    (add-option bend (turtle-pitch (pine-seed-angle x)))
    (add-option J (turtle-jump (pine-seed-displacement x)))
    (add-option seed (turtle-object "pine-seed" (pine-cone-size x)))))

(defun pine ()
  (cseed 127)
  (needles)
  (pine-cone)
  (with-graphics
    (iteration-count 15)
    (init-segments :count 12)
    (read-wavefront "needles.obj")
    (read-wavefront "pine-cone.obj")
    (assign-texture "branching" "pine-bark.png")
    (set-uv-calculator #'pine-uv-mapping)
    (add-option P (pine-wood age))
    (set-axiom (P(2) turn(5) P(1.5) turn(5) P(1)
		 turn(-10) P(0.5) turn(-5) P(0)
		 [ turn(-30) A(t) ] turn(10) A(t)))
    (add-rule (P(age) -> P((1+ age))))
    (add-rule ((delay-rip(x) ? (> x 0) -> delay-rip((1- x)))
	       (delay-rip(x) ? (= x 0) -> rip)))	       
    (add-rule ((cut(x) ? x -> )
	       (cut(x) ? (< 2 (crandom 5)) -> roll(-90))
	       (cut(x) -> delay-rip(1))))
    (add-rule (A(x) -> flip
		 [ cut((not x)) turn(-25) P(0.5) turn(-25) P(0) A((not x)) ]
		 [ cut(x) turn(30) P(0.5) turn(30) P(0) A((not x)) ]))))

(defparameter *freesia-green* (create-color 0.3 0.6 0.0))

(defun freesia-object (name x q)
  (turtle-jump (+ x q))
  (turtle-object name (+ x q)))

(defun freesia-organ (x y)
  (turtle-roll (* y 10))
  (let ((radius (state-scale *turtle*)))
    (dotimes (i 4)
      (turtle-set-radius (+ (* 2.0 0.25 (1+ i) radius) (* 0.333 (sqrt i))))
      (turtle-pitch 20)
      (turtle-forward 2)))
  (turtle-roll (* y 90))
  (turtle-object "cup" (+ x 3.2) *freesia-green*)
  (turtle-pitch 5)
  (turtle-roll (random 360))
  (cond ((> x 1.0) (freesia-object "flower2" x 7.0))
	((> x 0.5) (freesia-object "flower1" x 5.0))
	(t (freesia-object "bud" x 4.0))))

(defun freesia ()
  (with-graphics
    (iteration-count 11)
    (read-wavefront "freesia-cup.obj")
    (read-wavefront "freesia-bud.obj" "freesia-bud.png")
    (read-wavefront "freesia-flower1.obj" "freesia-flower.png")
    (read-wavefront "freesia-flower2.obj" "freesia-flower.png")
    (add-macro (flower(x) -> 
		      turn(-90) jump(-10) turn(90)
		      [ turn(-5) freesia(x) ] roll((+ 100 (random 20)))))
    (set-axiom (flower(0) flower(1) flower(2)))
    (add-rule ((freesia(x) ? (> x 0) -> freesia((1- x)))
	       (freesia(x) ? t -> color(*freesia-green*) roll(-90)
		       radius(1) stalk(4) stem(5) turn(-15)
		       [ organ(0 -1) ] cluster(1 0))))
    (add-option freesia-F (turtle-tropism x *gravity* 0.1))
    (add-rule (freesia-F(x) -> freesia-F(x)))
    (add-rule (cluster(x y) -> 
		      inc-radius(10) turn((* 30 x)) freesia-F((- 17 y))
		      [ organ(0 x) ] cluster((- x) (1+ y))))
    (add-rule ((stem(x) ? (> x 0) -> pitch(-13) F(3) stem((1- x)))))
    (add-rule ((stalk(x) ? (> x 0) -> roll(+fibonacci+) pitch(3)
		     freesia-F((+ 50 (random 50))) stalk((1- x)))))
    (add-option inc-radius (turtle-set-radius (* 0.05 x)))
    (add-rule (inc-radius(x) -> inc-radius((1+ x))))
    (add-option organ (freesia-organ (* 0.2 x) y))
    (add-rule (organ(x y) -> organ((1+ x) y)))))

(defun spruce-branch ()
  ; this l-system is used to produce spruce branch,
  ; it is rendered and applied as texture to produce low poly spruce
  (needles 100)
  (with-graphics    
    (iteration-count 10)
    (read-wavefront "needles.obj")
    (set-axiom (A(60)))
    (add-option R (turtle-turn x))
    (add-option S (turtle-turn x))
    (add-rule (X -> obj("skuja" 0.25) jump((+ 25 (random 5)))))
    (add-rule (S(x) -> S((* 1.04 x))))
    (add-rule (A(x) -> X X [ S((+ x)) B(x) ] [ S((- x)) B(x) ] A(x)))
    (add-rule (B(x) -> [ turn(-45) X ] [ turn(+45) X ] X B(x)))
    (add-rule (C(x) -> X B(x)))))

(defparameter *gray-wood* (color-scale (create-color 0.60 0.48 0.32) 0.5))

(defun low-poly-spruce ()
  ; this uses texture made from #'spruce-branch
  (with-graphics
    (iteration-count 60)
    (read-wavefront "spruce-branch.obj" "spruce-branch.png")
    (set-axiom (radius(15) color(*gray-wood*) F(600) radius(0.1) F(1) A(5)))
    (add-macro (B -> obj("spruce-branch" x)))
    (add-macro (M -> jump((* (+ x 20) -0.2))))
    (add-macro (P -> pitch((- (+ 15 (crandom 15)) x))))
    (add-rule (A(x) -> [ P B ] M roll(+fibonacci+) A((+ x 0.2))))))

(defun penrose-forward (x)
  ; we need to make each cylinder seperate, because penrose
  ; may draw forward then turn 180 and draw backwards
  ; this causes terrible artifacs when drawing continious cylinders
  (turtle-push)
  (turtle-forward x)
  (turtle-pop)
  (turtle-jump x))

(defun penrose ()
  (with-graphics
    (set-angle 36)
    (iteration-count 5)
    (add-option A (penrose-forward 25))
    (add-macro (init -> turn(1) jump(300) radius(2)))
    (set-axiom (init [ N ] + + [ N ] + + [ N ] + + [ N ] + + [ N ] ))
    (add-rule (M -> O A + + P A - - - - N A [ - O A - - - - M A ] + +))
    (add-rule (N -> + O A - - P A [ - - - M A - - N A ] +))
    (add-rule (O -> - M A + + N A [ + + + O A + + P A ] -))
    (add-rule (P -> - - O A + + + + M A [ + P A + + + + N A ] - - N A))
    (add-rule (A -> ))))

(defun elm-wood (w l)
  (turtle-set-surface w)
  (turtle-tropism l *gravity* (/ 0.05 w))
  (when (< w 3.0) 
    (turtle-push)
    (turtle-flip-up)
    (turtle-turn (- 60 (random 120)))
    (turtle-object "half-leaf" (+ 2.0 (* 0.2 (random 10))))
    (turtle-pop)))

(defun elm-angle ()
  (case (random 3)
    (0 +fibonacci+)
    (1 -157.50)
    (2 -112.50)))

(defun elm-structure ()
  (with-output-to-list
    (iteration-count 32)
    (set-axiom (wood apex(0 0)))
    (add-macro (side-shoot(x) -> [ turn!(x) roll!(45) wood A ]))
    (add-rule (wood -> wood))
    (add-rule (apex(i x) ? (< i 8) ->
		   apex((1+ i) (* 0.2 (+ x 90)))
		   wood roll!(+fibonacci+) side-shoot(x)))
    (add-macro (branch -> r-branch((random 3))))
    (add-rule ((r-branch(x) ? (> x 0) -> r-branch((1- x)))
	       (r-branch(x) -> [ pitch!((+ 10 (random 20))) wood A ])))
    (add-rule (A -> roll!((elm-angle)) wood branch roll!((elm-angle)) branch))))

(defun elm ()
  (with-graphics
    (iteration-count 33)
    (add-rule (wood -> w(0.1)))
    (assign-texture "branching" "bark.png")
    (read-wavefront "half-leaf.obj" "elm-leaf.png")
    (add-option w (elm-wood x (* 10.0 (expt x 0.2))))
    (use-axiom (filter-rules (elm-structure) '([ ] wood roll pitch turn)))
    (add-rule ((w(x) > [ w(s1) * ] [ w(s2) * ]	-> w((max x (+ s1 s2))))
	       (w(x) > [ w(s1) * ]   w(s2)	-> w((max x (+ s1 s2))))
	       (w(x) > [ w(s1) * ]		-> w((max x s1)))
	       (w(x) >   w(s1)			-> w((max x s1)))
	       (w(x)				-> w(x))))))

(defun straw (x)
  (turtle-push)
  (turtle-set-surface (* 0.2 x))
  (turtle-forward (* 6.0 x))
  (turtle-pop)
  (turtle-jump (* 6.0 x)))

(defun puzuris ()
  "traditional latvian christmas ornament made of reeds or straws"
  (with-graphics
    (iteration-count 5)
    (set-axiom (jump(650) roll(20) turn(180) puzurs))
    (add-option salms (straw x))
    (add-rule (salms(x) -> salms((* x (expt +golden-ratio+ 2)))))
    (add-rule (puzurs -> roll(90) [ puzura-mala ]
		         roll(90) [ puzura-mala ]
			 roll(90) [ puzura-mala ]
			 roll(90)   puzura-mala
			 pitch(-45) puzurs))
    (add-macro (puzura-mala -> pitch(-45) salms(1) pitch(45) 
			    [ roll(-45) turn(90) salms(1) ]
			    [ roll(45) puzurs ]
			    pitch(45) salms(1)))))

(defun crystal ()
  (with-graphics
    (iteration-count 20)
    (read-wavefront "crystal.obj")
    (add-macro (main-crystal -> [ pitch(-90) obj("crystal" 70 *green*) ]))
    (set-axiom (pitch(90) +(15) [ main-crystal A(0) ]))
    (add-rule (D(x) -> pitch((* -45 x)) jump(40)
		obj("crystal" (* 50 x) *green*)))
    (add-rule (A(x) ? (< x 20) -> +(+fibonacci+)
		[ jump((* 12 (sqrt x))) D((/ 1.0 (expt 1.1 x))) ]
		A((1+ x))))))

(defun cornflower ()
  (with-graphics
    (iteration-count 100)
    (read-wavefront "cornflower1.obj" "cornflower.png")
    (read-wavefront "cornflower2.obj" "cornflower.png")
    (read-wavefront "cornflower3.obj" "cornflower.png")
    (read-wavefront "cornflower4.obj" "cornflower.png")
    (set-axiom (color((create-color 0.15 0.28 0.16))
		radius(10) F(150) F(10)
		radius(15) F(20)
		radius(20) F(30)
		jump(-30) septal(1.0)))
    
    (add-rule ((septal(x) ? (< x 0) -> jump(100) flower(1.0))
	       (septal(x) ->
		       roll(+fibonacci+) jump((* (expt x 0.5) 5))
		       [ turn(-90) turn((+ (* 20 (- 1.0 x)) 50))
		         obj("septal" 30.0) ]
		       septal((- x 0.03)))))

    (add-rule ((flower(x) ? (< x 0) -> jump(-20) 
		      anther(1.0 "anther1" 0.1 50)
		      anther(1.0 "anther2" 0.01 100))
	       (flower(x) ->
		       roll(+fibonacci+)
		       [ turn(-100) jump((+ 30 (random 20)))
		         turn((* 50 (- 1.0 x))) roll((random 360))
		         obj("flower" 40.0) ]
		       flower((- x 0.03)))))

    (add-rule ((anther(x name density width) ? (< x 0) -> )
	       (anther(x name density width) ->
		       roll(+fibonacci+)
		       [ pitch(+90) jump((* (- x 1.0) width))
		         pitch(-20) obj(name 20.0) ]
		       anther((- x density) name density width))))))

(defun capilar (x)
  (let ((q (noise (point-scale (state-location *turtle*) 5))))
    (turtle-set-radius x)
    (turtle-turn (* 180 (- 0.5 q)))
    (turtle-forward (* 4 (expt x 0.5)))))

(defun capilar-width1 (x y)
  (* x y))

(defparameter *capilar-constant* 2.7) ; from digital design of nature

(defun capilar-width2 (x y)
  (expt (- (expt x *capilar-constant*)
	   (expt (capilar-width1 x y) *capilar-constant*))
	(/ 1 *capilar-constant*)))

(defun capilar-vary ()
  (+ 0.6 (* 0.3 (* 0.001 (crandom 1000)))))

(defun blood-vessels ()
  (cseed 3)
  (with-graphics
    (iteration-count 100)
    (add-rule (C(x) -> C(x)))
    (add-option C (capilar x))
    (set-axiom (color((create-color 0.5 0.0 0.0)) B(10 0.5)))
    (add-rule ((B(x y) ? (or (< x 2) (/= 0 (crandom 10)))
		 -> C(x) B(x (capilar-vary)))
	       (B(x y) -> C(x)
		 [ turn(+30) B((capilar-width1 x y) (capilar-vary)) ]
		 [ turn(-30) B((capilar-width2 x y) (capilar-vary)) ])))))

(defun all ()
  (gosper)
  (figure-1.39)
  (figure-1.31-d)
  (figure-1.25)
  (figure-1.26)
  (figure-2.6-d)
  (figure-2.7-d)
  (figure-2.8-c)
  (figure-3.5)
  (figure-3.19)
  (figure-4.1)
  (figure-4.11)
  (figure-5.8)
  (monkey-tree)
  (bacopa)
  (patch-of-grass)
  (anubias)
  (egle)
  (birch)
  (rose)
  (fikuss)
  (barley)
  (maple)
  (chestnut)
  (dead-tree)
  (oak)
  (spruce)
  (dandelion)
  (water-lily)
  (pine)
  (freesia)
  (elm)
  (penrose)
  (puzuris)
  (crystal))

(defvar *call-count* 0)

#+sbcl
(defun abop-rib ()
  (handler-case
      (loop
	(incf *call-count*)
	(when (= 0 (mod *call-count* 100))
	  (format *error-output* "plants = ~A~%" *call-count*))
	(let ((stream (make-string-input-stream (read-line))))
	  (read stream) ; skip detail size
	  (cseed (read stream)))	 
	 (format t "Rotate ~A 0.0 1.0 0.0~%" (crandom 360))
	 (with-output-to-stdio (low-poly-spruce))
	 (emit_char 255))
    (condition (var) (format *error-output* "~A~%" var))))

#+sbcl
(defun save-and-die ()
  (sb-ext::save-lisp-and-die 
   "abop-rib" :toplevel #'abop::abop-rib :executable t))
