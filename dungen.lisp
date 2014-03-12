;;;; dungen.lisp
;; Copyright (c) 2014 Kalman Kiss, Zalaegerszeg Hungary
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; Author and contact:
;; Kalman Kiss <kiskami@freemail.hu>
;; 8900 Zalaegerszeg, Hungary
;; Kakukkfu u. 4.

(in-package #:firstrl)

(defconstant +LEVEL_MAXH+ 25)
(defconstant +CORRIDOR_MAXLEN+ 5 "max corridor length - make a turn/room after this")
(defconstant +STEPS_DIVIDER+ 1)

(defstruct room
  x y w h
  )

(defun get-leveldata-name (&key name 
			     (w +WINDOW-W+) (h +LEVEL_MAXH+) 
			     (roomcount 5) 
			     minw minh maxw maxh
			     (parents nil) (childs nil)
			     )
  name)

(defun get-leveldata-parents (&key name 
			     (w +WINDOW-W+) (h +LEVEL_MAXH+) 
			     (roomcount 5) 
			     minw minh maxw maxh
			     (parents nil) (childs nil)
			     )
  parents)

(defun get-leveldata-childs (&key name 
			     (w +WINDOW-W+) (h +LEVEL_MAXH+) 
			     (roomcount 5) 
			     minw minh maxw maxh
			     (parents nil) (childs nil)
			     )
  childs)

(defun gen-level* (&key (name "[unnamed]") 
		     (w +WINDOW-W+) (h +LEVEL_MAXH+) 
		     (roomcount 5) 
		     minw minh maxw maxh
		     (parents nil) (childs nil))
  (gen-level w h roomcount minw minh maxw maxh))

(defun gen-features (leveldata map)
  (let* ((result ())
	 (parents (apply #'get-leveldata-parents leveldata))
	 (parent-ladder-kords (gen-parent-ladder-kords (length parents) map))
	 (ladderup (get-dungeonfeaturedata ">"))
	 (i 0)
	 )
    (dolist (p parents)
      (setf result (append result (list (make-object :name (dungeonfeaturedata-name ladderup)
						:typeid ">"
						:x (car (nth i parent-ladder-kords))
						:y (cdr (nth i parent-ladder-kords))
						:state p
						:thinkfunc nil))))
      (incf i))
    result))

(defun get-dungeonfeaturedata (typeid)
  (gethash typeid *dungeonfeaturedata*))

(defun gen-parent-ladder-kords_ (cnt map)
  (let ((res nil)
	(kord nil))
    (when (> cnt 0)
      ;; only one parent supported atm
      )
    res))

(defun gen-parent-ladder-kords (cnt map)
  (let ((res nil)
	(kord nil))
    (when (> cnt 0)
      ;; only one parent supported atm
      (loop for x from 0 to (1- (array-dimension map 0))
	 do
	   (loop for y from 0 to (1- (array-dimension map 1))
	      do
		(when (is-floor map x y)
		  (setf kord (cons x y))
		  (return)))
	   (when kord (return)))
      (format t "parent-ladder-kord ~A~%" kord)
      (setf res (append res (list kord))))
    res))

(defun gen-level (levelw levelh roomcount minw minh maxw maxh 
		  &key (maxcorrlen +CORRIDOR_MAXLEN+) (stepsdivider +STEPS_DIVIDER+))
    "from http://www.roguebasin.com/index.php?title=Diffusion-limited_aggregation
Pseudocode for block aggregation

- Fill a level with wall
- Dig a seed tile or block, probably in the centre
- Do until the level is considered sufficiently full (predefined number 
  of blocks, edge proximity or number of tiles dug):
  - Choose a block, either from a given set or using a function
  - Choose a location from which to begin the block's walk
    If necessary, limit this choice to locations that don't interfere 
    with already-dug sections
  - Do until the block neighbours or collides with the dug-out section:
    Move the block one step in a random direction 
  - Dig out the tiles covered by the block 
  - Add any other desired features, eg. extra corridors to increase connectivity "
  (let ((level (make-array (list levelw levelh) :element-type 'character :initial-element #\Space))
	(room (gen-random-room 1 1 (- levelw maxw 2) (- levelh maxh 2) minw minh maxw maxh)) ; first room
	(roomcnt 1)
	(maxsteps (/ (* levelw levelh) stepsdivider)) ; max generation steps for full level
	(builderok nil)			; builder is alive (useable)?
	(bx 0) (by 0)			; builder x and y coords
	(corrlen 0)
	(roomstart t)
	(movedir 0)
	(maxx (- levelw 2)) (maxy (- levelh 2))
	(lastbx 0) (lastby 0)
	)
    (setf (room-x room) (floor (/ levelw 2))
	  (room-y room) (floor (/ levelh 2)))
    (add-room level
	      (room-x room) (room-y room) 
	      (room-w room) (room-h room))
    (format t "Generation level in max ~A steps.~%" maxsteps)
    (do ((steps 1 (1+ steps)))
    	((or (= roomcnt roomcount) (> steps maxsteps)) level)
;      (format t "~A~%" (debug-format-level level))
      (setf lastbx bx
	    lastby by)
      (cond (builderok
	     (ecase movedir
	       (1 ; N
		(when (> by 0)
		  (decf by)))
	       (2 ; E
		(when (< bx maxx)
		  (incf bx)))
	       (3 ; S
		(when (< by maxy)
		  (incf by)))
	       (4 ; W
		(when (< 0 bx)
		  (decf bx)))
	       (5 ; NE
		(when (and (> by 0) (> maxx bx))
		  (decf by) (incf bx)))
	       (6 ; SE
		(when (and (< bx maxx) (< by maxy))
		  (incf bx) (incf by)))
	       (7 ; SW
		(when (and (> maxy by) (< 0 bx))
		  (decf bx) (incf by)))
	       (8 ; NW
		(when (and (< 0 bx) (< 0 by))
		  (decf bx) (decf by)))
	       )
	     (incf corrlen)
	     (cond ((and 
		     (< 1 bx) (> maxx bx)
		     (< 1 by) (> maxy by)
					;		  (not (is-floor level bx by))
		     (< corrlen maxcorrlen))
					;		 (when (touching-corridor level bx by)
		    (plot-corridor level bx by))
;		   )
		   (t 
		    (setf builderok nil
			  corrlen 0))))
    	    (t
	     (cond (roomstart
		    (format t "roomstart~%")
		    (let ((kord (rnd-side-koord room)))
		      (setf bx (first kord)
			    by (second kord))
		      (when (and (> bx 0) (> by 0)
				 (< bx maxx) (< by maxy))
			(plot-corridor level bx by)
			(setf builderok t
			      roomstart nil
			      movedir (get-rnd-number 1 4))
			(incf corrlen))))
		   (t
		    (format t "corridor start~%")
		    ;; (loop do
		    ;; 	 (setf bx (get-rnd-number 2 (1- maxx))
		    ;; 	       by (get-rnd-number 2 (1- maxy)))
		    ;;    while (not (is-floor level bx by #\#)))
		    ;; (setf bx (get-rnd-number 2 (1- maxx))
		    ;; 	  by (get-rnd-number 2 (1- maxy)))
		    (setf bx lastbx
			  by lastby)
		      (when (and (> bx 0) (> by 0)
				 (< bx maxx) (< by maxy))
			(plot-corridor level bx by))
		      (setf movedir (get-rnd-number 1 4)
			    corrlen 0
			    builderok t)
		      (incf corrlen)))))
    level)))

(defun is-floor (level x y &optional (char #\.))
  (eq char (aref level x y)))

(defparameter dkords '((-1 . -1) (0 . -1) (1 . -1)
		       (-1 . 0)  (1 . 0)
		       (-1 . 1)  (0 . 1)  (1 . 1)))

(defun touching-corridor (level x y)
  (dolist (dc dkords)
    (when (is-floor level (+ x (car dc)) (+ y (cdr dc)))
      (return-from touching-corridor t)))
  nil)

(defun plot-corridor (level x y)
  (format t "plot-corridor @ ~A,~A~%" x y)
  (setf (aref level x y) #\.)
  (dolist (dc dkords)
    (when (not (is-floor level (+ x (car dc)) (+ y (cdr dc))))
      (setf (aref level (+ x (car dc)) (+ y (cdr dc))) #\#))))

(defun rnd-side-koord (room)
  (let ((x -1) (y -1))
    (case (get-rnd-number 0 3)
      (0 ; upper side
       (setf y (room-y room))
       (setf x (get-rnd-number (room-x room) (+ (room-x room) (room-w room) 1)))
       )
      (1 ; right side
       (setf x (+ (room-x room) (room-w room) 1))
       (setf y (get-rnd-number (room-y room) (+ (room-y room) (room-h room) 1)))
       )
      (2 ; bottom side
       (setf y (+ (room-y room) (room-h room) 1))
       (setf x (get-rnd-number (room-x room) (+ (room-x room) (room-w room) 1))))
      (3 ; left side
       (setf x (room-x room))
       (setf y (get-rnd-number (room-y room) (+ (room-y room) (room-h room) 1)))
       ))
    (format t "rnd-side-koord (~A,~A)~%" x y)
    (list x y)
  ))

(defun gen-random-room (minx miny maxx maxy minw minh maxw maxh)
  (let ((r (gen-random-room_ minx miny maxx maxy minw minh maxw maxh)))
    (make-room :x (first r) :y (second r) :w (third r) :h (fourth r))))

(defun gen-random-room_ (minx miny maxx maxy minw minh maxw maxh)
  (let ((x (get-rnd-number minx maxx))
	(y (get-rnd-number miny maxy))
	(w (get-rnd-number minw maxw))
	(h (get-rnd-number minh maxh)))
    (format t "gen-random-room @ (~A,~A) ~Ax~A~%" x y w h)
    (list x y w h)))

(defun add-room (level x y w h)
  (let ((cnt 0))
    (loop for xi from x to (+ x w 1) do
	 (loop for yi from y to (+ y h 1) do
	      (cond ((or (= xi x)
			 (= xi (+ x w 1))
			 (= yi y)
			(= yi (+ y h 1)))
		     (when (eq #\Space (aref level xi yi))
		       (setf (aref level xi yi) #\#)
		       ))
		    (t
		     (setf (aref level xi yi) #\.)
		     (incf cnt)))))
    cnt))

(defun debug-format-level (level)
  (let ((line ""))
    (loop for yi from 0 to (1- (array-dimension level 1)) do
	 (loop for xi from 0 to (1- (array-dimension level 0)) do
	      (setf line (format nil "~A~A" line (aref level xi yi))))
	 (setf line (format nil "~A~%" line)))
    line))
