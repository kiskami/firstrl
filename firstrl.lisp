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
				    :levels (gen-levels player)
				    :turns 0))
		     (spawn-player player dungeon)
		     (draw-level console (get-player-level dungeon))
		     (update-console console))
		    'indungeon)
		   ('indungeon
		    (format t "in dungeon...~%")
		    (do-update-dungeon console dungeon))
		   ('death
		    (format t "death...~%")
		    (do-death console dungeon))
		   ('win
		    (format t "win...~%")
		    (do-win console dungeon))
		   ('end
		    (setf should-exit t))
		   (otherwise
		    (format t "starting...~%")
		    (do-intro console)))))))
  (format t "exiting...~%"))

(defun init-game-data ()
  (format t "initializing game data...~%")
  (dolist (m +MONSTERSDATA+)
    (let ((monsta (apply #'make-monsterdata m)))
      (setf (gethash (monsterdata-typeid monsta) *monsterdata*) monsta)))
  (dolist (i +ITEMSDATA+)
    (let ((item (apply #'make-itemdata i)))
      (setf (gethash (itemdata-typeid item) *itemdata*) item)))
  (dolist (f +DUNGEONFEATURESDATA+)
    (let ((feature (apply #'make-dungeonfeaturedata f)))
      (setf (gethash (dungeonfeaturedata-typeid feature) *dungeonfeaturedata*) feature)))

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
   :name "teszt player"
   :gender 'm
   :role 'knight
   :alignment 'chaothic-good
   :deity 'zumba
   :inventory '()
   :armor '()
   :weapons '()
   :tools '()
   :hp 100 :maxhp 100
   :aktlevel 0))

(defun gen-levels (player)
  "Generate dungeon levels for player."
  (list 
   (create-level (nth 0 +LEVELDATA+))
   (create-level (nth 1 +LEVELDATA+))
   (convert-test-level "test level" +TESTLEVEL+)))

(defun create-level (leveldata)
  (let ((map (apply #'gen-level* (nth 0 +LEVELDATA+))))
    (make-level
     :name (apply #'get-leveldata-name leveldata)
     :parents (apply #'get-leveldata-parents leveldata)
     :childs (apply #'get-leveldata-childs leveldata)
     :features (gen-features leveldata map) ; first gen the features
     :items ()				; next gen the items
     :monsters ()			; last gen the monsters
     :map map)))

(defun get-player-level (dungeon)
  (nth (lifeform-aktlevel (dungeon-player dungeon)) (dungeon-levels dungeon)))

(defun spawn-player (player dungeon)
  "Place player on the first level in dungeon by the ladder upwards (entry)."
  (setf (lifeform-aktlevel player) 0)
  (let ((upladders (find-dungeonfeatures-in-level (get-player-level dungeon) ">")))
    (when upladders 
      (setf (lifeform-x player) (object-x (first upladders)))
      (setf (lifeform-y player) (object-y (first upladders))))))

(defun do-update-dungeon (console dungeon)
  (let ((key (wait-for-any-key console)))
    (clear-console console)
    (update-console console)
    (display-text console 55 33 "firstrl - Copyright (C) 2014 Kalman Kiss" :font 'sans-bold)
    (cond ((key-eq key +key-q+)
	   'end)
	  ((key-eq key +key-space+)
	   (format t "*")
	   (player-idle-turn console dungeon)
	   (update-console console)
	   'indungeon)
	  ((key-eq key +key-h+)
	   (format t "help~%")
	   (display-help console)
	   'indungeon))))

(defun player-idle-turn (console dungeon)
  (incf (dungeon-turns dungeon))
  (let ((player (dungeon-player dungeon))
	(level (get-player-level dungeon)))
   (update-idle-player player)
   (update-level level)
   (draw-level console level)
   (draw-player console player))
  )

(defun display-help (console))

(defun do-death (console dungeon)
  'end)

(defun do-win (console dungeon)
  'end)

(defun update-idle-player (player))

(defun update-level (level))

(defun draw-level (console level)
  (draw-map console (level-map level))
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

(defun draw-player (console player &key (dx 1) (dy 1))
  (display-text console 0 27 "1")
  (display-text console 0 28 "2")
  (display-text console 0 29 "3")
  (display-text console 0 30 "4")
  (display-text console 0 31 "5")
  (display-text console 0 32 "6")
  (display-text console 0 33 "7")
  (display-text console (+ dx (lifeform-x player)) (+ dy (lifeform-y player)) (playerdata-char *playerdata*))
  )
