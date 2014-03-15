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
		     (draw-level console (get-player-level dungeon))
		     (draw-player console player)
		     (add-msg console (format nil "Welcome to the ~A!" (level-name (get-player-level dungeon))))
		     (update-console console))
		    'indungeon)
		   ('indungeon
		    (format t "in dungeon...~%")
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
  (display-text-wrapped console 20 15 40 10 +INTRO-TEXT+)
  (update-console console)
  (wait-for-keypress +key-space+ console)
  (clear-console console)
  (update-console console)
  'chargen)

(defun do-chargen (console)
  "Player character generation."
  (make-lifeform
   :name "AXCSDAVF ASRF"
   :typeid "@"
   :aktlevel 0
   :state 'alive
   :turns 0
   
   :role 'scamp
   :hp 10 :maxhp 10
   :power 1 :maxpower 1
   :str 1 :maxstr 1
   :dex 1 :maxdex 1
   :con 1 :maxcon 1
   :int 1 :maxint 1
   :wis 1 :maxwis 1
   :cha 1 :maxcha 1
   
   :gender 'm
   :deity 'zumba
   :armor ()
   :weapons ()
   :tools ()
   :magic ()
   :alignment 'chaothic-good
   :xp 0
   :purse 1
   :inventory ()
   :att 1 :def 1 :spe 1
   ))

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
     :map map)))

(defun gen-items (leveldata map features))

(defun spawn-player (player dungeon)
  "Place player on the first level in dungeon by the ladder upwards (entry)."
  (setf (lifeform-aktlevel player) 0)
  (let ((upladders (find-dungeonfeatures-in-level (get-player-level dungeon) ">")))
    (when upladders 
      (setf (lifeform-x player) (object-x (first upladders)))
      (setf (lifeform-y player) (object-y (first upladders))))))

(defun display-copyright (console)
    (display-text console 55 33 "firstrl - Copyright (C) 2014 Kalman Kiss" :font 'sans-bold))

(defun do-update-dungeon (console dungeon)
  (let ((key (wait-for-any-key console))
	(res 'indungeon)
	(player (dungeon-player dungeon))
	(level (get-player-level dungeon))
	)
    (clear-console console)
;    (update-console console)
    (display-copyright console)
    (cond ((key-eq key +key-q+)
	   (setf res 'end))
	  ((key-eq key +key-.+)
	   (format t ".")
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
	  ((key-eq key +key-<+)
	   (move-player-down console dungeon))
	  ((key-eq key +key->+)
	   (move-player-up console dungeon))
	  )
    (update-level level)
    (draw-level console level)
    (draw-player console player)
    (update-console console)
    res))

(defun player-idle-turn (console dungeon)
  (let ((player (dungeon-player dungeon))
	(level (get-player-level dungeon)))
   )
  )

(defun move-player-down (console dungeon))

(defun move-player-up (console dungeon))

(defun display-help (console))

(defun do-death (console dungeon)
  'end)

(defun do-win (console dungeon)
  'end)

(defun update-idle-player (player)
    (incf (lifeform-turns player)))

(defun update-level (level)
  (dolist (m (level-monsters level))
    (when (lifeform-thinkfunc m)
      (funcall (lifeform-thinkfunc m) 'update)))
  (dolist (i (level-items level))
    (when (object-thinkfunc i)
      (funcall (object-thinkfunc i) 'update)))
  )

(defun draw-level (console level)
  (draw-map console (level-map level))
  (draw-features console (level-features level))
  (draw-items console (level-items level))
  (draw-monsters console (level-monsters level)))

(defun draw-map (console map &key (dx 1) (dy 1))
  (loop for y from 0 to (1- (array-dimension map 1))
     do (loop for x from 0 to (1- (array-dimension map 0))
	   do (when (not (eq #\Space (aref map x y)))
		(display-char-glyp console (+ x dx) (+ y dy) (string (aref map x y)))
		))))

 (defun draw-items (console items &key (dx 1) (dy 1))
   (dolist (i items)
     (display-char-glyp console (+ dx (object-x i)) (+ dy (object-y i))
		   (string (itemdata-char (gethash (object-typeid i) *itemdata*))))))

(defun draw-monsters (console monstas &key (dx 1) (dy 1))
  (dolist (m monstas)
    (display-char-glyp console (+ dx (object-x m)) (+ dy (object-y m)) 
		       (string (monsterdata-char (gethash (object-typeid m) *monsterdata*))))))

(defun draw-features (console features &key (dx 1) (dy 1))
  (dolist (f features)
    (display-char-glyp console (+ dx (object-x f)) (+ dy (object-y f)) 
		       (string (dungeonfeaturedata-char (gethash (object-typeid f) *dungeonfeaturedata*))))))
