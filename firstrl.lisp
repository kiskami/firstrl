;;;; firstrl.lisp
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

(defun run (&optional (w +WINDOW-W+) (h +WINDOW-H+))
  (format t "~A~%" +GAME-LABEL+)
  (with-init-console ()
    (let ((console (create-window w h "firstrl"))
	  (should-exit nil)
	  (gamestate nil)
	  (dungeon nil))
      (init-game-data)
      (loop until should-exit do
	   (setf gamestate 
		 (case gamestate
		   ('chargen
		    (format t "chargen...~%")
		    (let ((player (do-chargen console)))
		     (setf dungeon (make-dungeon
				    :player player
				    :levels (gen-levels player)))
		     (spawn-player player dungeon)
		     (display-copyright console)
		     (draw-level console (get-player-level dungeon) player)
		     (welcome-to-level console (get-player-level dungeon))
		     (draw-player console player)
		     (update-console console))
		    'indungeon)
		   ('indungeon
		    ;; (format t "in dungeon...~A~%" 
		    ;; 	    (lifeform-turns (dungeon-player dungeon)))
		    (do-update-dungeon console dungeon))
		   ('death
		    (format t "death...~%")
		    (do-death console dungeon)
		    'end)
		   ('win
		    (format t "win...~%")
		    (do-win console dungeon)
		    'end)
		   ('end
		    (setf should-exit t))
		   (otherwise
		    (format t "starting...~%")
		    (do-intro console)))))))
  (format t "exiting...~%"))

(defun init-game-data ()
  (format t "initializing game data...~%")
  (setf *RNDSTATE* (make-random-state t))
  (dolist (m +MONSTERSDATA+)
    (let ((monsta (apply #'make-monsterdata m)))
      (setf (gethash (monsterdata-typeid monsta) *monsterdata*) monsta)))
  (dolist (i +ITEMSDATA+)
    (let ((item (apply #'make-itemdata i)))
      (setf (gethash (itemdata-typeid item) *itemdata*) item)))
  (dolist (f +DUNGEONFEATURESDATA+)
    (let ((feature (apply #'make-dungeonfeaturedata f)))
      (setf (gethash (dungeonfeaturedata-typeid feature) *dungeonfeaturedata*) feature)))
  (dotimes (i (length +LEVELDATA+))
    (let ((leveldata (apply #'make-leveldata (nth i +LEVELDATA+))))
      (setf (gethash i *leveldata*) leveldata))
    )
  (setf *playerdata* (apply #'make-playerdata +PLAYERDATA+))
  (init-glyp-cache))

(defun init-glyp-cache ()
  (maphash #'(lambda (k v)
	       (cache-char-glyp (dungeonfeaturedata-char v) 
;				:font (funcall get-dungeonfeature-fontname f)
				:color +GRAY-COLOR+))
	   *dungeonfeaturedata*)
  (maphash #'(lambda (k v)
	       (cache-char-glyp (itemdata-char v)
;				:font (funcall get-item-fontname i)
				))
	   *itemdata*)
  (maphash #'(lambda (k v)
	       (cache-char-glyp (monsterdata-char v)
;			    :font (funcall get-monster-fontname m)
				))
	   *monsterdata*)

  (cache-char-glyp (playerdata-char *playerdata*)
					;		   :font (funcall get-player-fontname +PLAYER+)
		   ))

(defun do-intro (console)
  "Display intro and wait for key to proceed to chargen."
  (clear-console console)
  (display-copyright console)
  (display-text-wrapped console 20 15 40 10 +INTRO-TEXT+)
  (update-console console)
  (wait-for-keypress +key-space+ console)
  (clear-console console)
  (update-console console)
  'chargen)

(defun do-chargen (console)
  "Player character generation."
  (let ((c (make-lifeform
	    :name "AXCSDAVF ASRF" :typeid "@" :aktlevel 0 :state 'alive :turns 0
	    :role 'scamp
	    :hp 0 :maxhp 0 :power 1 :maxpower 1
	    :str 1 :maxstr 1 :dex 1 :maxdex 1
	    :con 1 :maxcon 1 :int 1 :maxint 1
	    :wis 1 :maxwis 1 :cha 1 :maxcha 1
	    :gender 'm :deity 'zumba
	    :armor ()
	    :weapons ()
	    :tools ()
	    :magic ()
	    :alignment 'chaothic-good :xp 0 :xplevel -1 
	    :purse 1 :inventory ()
	    :att 0 :def 0 :spe 0
	    )))
    (advance-player-xp c 0)
    c))

(defun gen-levels (player)
  "Generate dungeon levels for player."
  (list
   (create-level (gethash 0 *leveldata*))
   (create-level (gethash 1 *leveldata*))
   (create-level (gethash 2 *leveldata*))
   (create-level (gethash 3 *leveldata*))
   (create-level (gethash 4 *leveldata*))
;   (convert-test-level "test level" +TESTLEVEL+)
   ))

(defun create-level (leveldata)
  (let* ((map (gen-level* leveldata))
	 (f (gen-features leveldata map)) ; first gen the features
	 (i (gen-items leveldata map f))  ; next gen the items
	 (m (gen-monsters leveldata map f i)) ; last gen the monsters
	 )
    (make-level
     :name (leveldata-name leveldata)
     :parents (leveldata-parents leveldata)
     :childs (leveldata-childs leveldata)
     :features f
     :items i
     :monsters m
     :map map
     :mapmask (make-array (array-dimensions map) :element-type 'character :initial-element #\0)
     )))

(defun spawn-player (player dungeon &optional (level 0) (ud 'down))
  "Place player on the level in dungeon by the ladder upwards (entry)."
  (setf (lifeform-aktlevel player) level)
  (let ((ladder (find-dungeonfeatures-in-level (get-player-level dungeon)
					       (if (equal ud 'down)
						">"
						"<"))))
    (when ladder 
      (setf (lifeform-x player) (object-x (first ladder)))
      (setf (lifeform-y player) (object-y (first ladder))))))

(defun display-copyright (console)
    (display-text console 55 33 "firstrl - Copyright (C) 2014 Kalman Kiss" :font 'sans-bold))

(defun welcome-to-level (console level)
  (add-msg (format nil "Welcome to the ~A!" (level-name level)))
  (display-msg-window console +MSGWIND-X+ +MSGWIND-Y+))

(defun do-update-dungeon (console dungeon)
  (let ((key (wait-for-any-key console))
	(res 'indungeon)
	(player (dungeon-player dungeon))
	(level (get-player-level dungeon))
	)
    (clear-console console)
    (display-copyright console)
;    (format t "pressed key ~A~%" key)
    (cond ((key-eq key +key-q+)
	   (setf res 'end))
	  ((key-eq key +key-?+)
	   (help console))
	  ((key-eq key +key-.+)
;	   (format t ".")
	   (update-idle-player player))
	  ;; player movement input
	  ((key-eq key +key-y+)
	   (move-player console dungeon 0))
	  ((key-eq key +key-k+)
	   (move-player console dungeon 1))
	  ((key-eq key +key-u+)
	   (move-player console dungeon 2))
	  ((key-eq key +key-h+)
	   (move-player console dungeon 3))
	  ((key-eq key +key-l+)
	   (move-player console dungeon 4))
	  ((key-eq key +key-b+)
	   (move-player console dungeon 5))
	  ((key-eq key +key-j+)
	   (move-player console dungeon 6))
	  ((key-eq key +key-n+)
	   (move-player console dungeon 7))
	  ((key-eq key +key-e+)
	   (player-eat dungeon))
	  ((key-eq key +key-comma+)
	   (player-pickup dungeon))
	  ((key-eq key +key-<+)
;	   (format t "key < pressed~%")
	   (move-player-down console dungeon)
	   (setf level (get-player-level dungeon))
	   )
	  ((or (key-eq key +key->+)
	       ;; horrible hack: on my international keyboard
	       ;; ">" (Alt Gr Y) is detected as "Z"
	       (key-eq key +key-z+))
;	   (format t "key > pressed~%")
	   (setf res (move-player-up console dungeon)
		 level (get-player-level dungeon)
		 ))
	  )
    (update-level level)
    (draw-level console level player)
    (draw-player console player)
    (update-console console)
    (when (not (player-alive player))
      (setf res 'death))
    ;; last levels last monster dead - you WON
    (when (not (monsta-alive
		(first (last (level-monsters (first (last (dungeon-levels dungeon))))))))
      (setf res 'win))
    res))

(defun help (console))

(defun move-player-up/down (console dungeon typeid ud)
  (let* ((level (get-player-level dungeon))
	 (player (dungeon-player dungeon))
	 (f (is-object-at (lifeform-x player) (lifeform-y player)
			  (level-features level))))
    (when (and f (equal typeid (object-typeid f)))
      (return-from move-player-up/down
	(switch-level console dungeon (object-state f) ud)))
    )
  'indungeon
  )

(defun move-player-up (console dungeon)
  (move-player-up/down console dungeon ">" 'up)
  )

(defun move-player-down (console dungeon)
  (move-player-up/down console dungeon "<" 'down)
  )

(defun switch-level (console dungeon n ud)
  (cond ((not (equal n 'end))
	 (spawn-player (dungeon-player dungeon) dungeon n ud)
	 (welcome-to-level console (get-player-level dungeon))
	 'indungeon
	 )
	(t
	 (format t "exiting the dungeon~%")
	 'end)
	)
  )

(defun display-help (console))

(defun do-death (console dungeon)
  (add-msg (format nil "*** You DIEd. Embarassing. Press any key to quit. ***"))
  (let ((player (dungeon-player dungeon))
	(level (get-player-level dungeon)))
    (clear-console console)
    (draw-level console level player)
    (draw-player console player))
  (update-console console)
  (wait-for-any-key console)
  'end)

(defun do-win (console dungeon)
  (add-msg (format nil "*** You WON. Is this even possible? Embarassing. ***"))
  (add-msg "Press any key to quit.")
  (let ((player (dungeon-player dungeon))
	(level (get-player-level dungeon)))
    (clear-console console)
    (draw-level console level player)
    (draw-player console player))
  (update-console console)
  (wait-for-any-key console)
  'end)

(defun update-idle-player (player)
    (incf (lifeform-turns player)))

(defun update-level (level)
  (dolist (m (level-monsters level))
    (when (lifeform-thinkfunc m)
      (funcall (lifeform-thinkfunc m) m 'update)))
  (dolist (i (level-items level))
    (when (object-thinkfunc i)
      (funcall (object-thinkfunc i) i 'update)))
  )

(defun draw-level (console level player)
  (let ((px (lifeform-x player))
	(py (lifeform-y player)))
    (draw-map console (level-map level) (level-mapmask level) px py)
    (draw-features console (level-features level) px py)
    (draw-items console (level-items level) px py)
    (draw-monsters console (level-monsters level) px py)))

(defun draw-map_ (console map px py &key (dx 1) (dy 1) (eyeshot 2))
  (dolist (kord (if (> eyeshot 1) (append dkords dkords2) dkords))
    (let ((x (+ px (car kord)))
	  (y (+ py (cdr kord))))
      (when (and 
	     (>= x 0) (>= y 0)
	     (<= x (1- (array-dimension map 0))) (<= y (1- (array-dimension map 1)))
	     (not (eq #\Space (aref map x y)))
	     )
	(display-char-glyp console (+ x dx) (+ y dy) (string (aref map x y))))))
  )

(defun draw-map (console map mapmask px py &key (dx 1) (dy 1) (eyeshot 2)) 
  (loop for y from 0 to (1- (array-dimension map 1))
     do (loop for x from 0 to (1- (array-dimension map 0))
	   do (when (and 
		     (or (equal #\1 (aref mapmask x y)) 
			 (within-eyeshot px py eyeshot x y)) 
		     (not (eq #\Space (aref map x y))))
		(setf (aref mapmask x y) #\1)
		(display-char-glyp console (+ x dx) (+ y dy) (string (aref map x y)))
		))))

(defun draw-items (console items px py &key (dx 1) (dy 1) (eyeshot 2))
  (dolist (i items)
    (when (within-eyeshot px py eyeshot (object-x i) (object-y i))
      (display-char-glyp console (+ dx (object-x i)) (+ dy (object-y i))
			 (string (itemdata-char (gethash (object-typeid i) *itemdata*)))))))

(defun draw-monsters (console monstas px py &key (dx 1) (dy 1) (eyeshot 2))
  (dolist (m monstas)
    (when (within-eyeshot px py eyeshot (object-x m) (object-y m))
      (display-char-glyp console (+ dx (object-x m)) (+ dy (object-y m))
			 (string (monsterdata-char (gethash (object-typeid m) *monsterdata*)))))))

(defun draw-features (console features px py &key (dx 1) (dy 1) (eyeshot 2))
  (dolist (f features)
    (when (within-eyeshot px py eyeshot (object-x f) (object-y f))
      (display-char-glyp console (+ dx (object-x f)) (+ dy (object-y f))
			 (string (dungeonfeaturedata-char (gethash (object-typeid f) *dungeonfeaturedata*)))))))

(defun within-eyeshot (px py eyeshot ox oy)
  (dolist (kord (if (> eyeshot 1) (append dkords dkords2) dkords))
    (when (and (= ox (+ px (car kord)))
	       (= oy (+ py (cdr kord))))
      (return-from within-eyeshot t)))
  nil)
