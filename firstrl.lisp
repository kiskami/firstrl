;;;; firstrl.lisp

(in-package #:firstrl)

(defun run (&optional (w +WINDOW-W+) (h +WINDOW-H+))
  (format t "~A~%" +GAME-LABEL+)
  (with-init-console ()
    (let ((console (create-window w h "firstrl"))
	  (should-exit nil)
	  (gamestate nil)
	  (dungeon nil))
      (loop until should-exit do
	   (setf gamestate 
		 (case gamestate
		   ('chargen
		    (format t "chargen...~%")
		    (let ((player (do-chargen console)))
		     (setf dungeon (make-dungeon
				    :player player
				    :levels (gen-levels player)
				    :turns 0)))
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

(defun do-intro (console)
  "Display intro and wait for key to proceed to chargen."
  (clear-console console)
  (display-text-wrapped console 13 15 40 10 +INTRO-TEXT+)
  (update-console console)
  (wait-for-keypress +key-space+ console)
  (clear-console console)
  (update-console console)
  'chargen)

(defun do-chargen (console)
  (make-player 
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
  (list (convert-test-level "test level" +TESTLEVEL+)))

(defun do-update-dungeon (console dungeon)
  (let ((key (wait-for-any-key console)))
    (clear-console console)
    (update-console console)
    (cond ((key-eq key +key-q+)
	   'end)
	  ((key-eq key +key-space+)
	   (format t "*")
	   (make-turn dungeon)
	   'indungeon)
	  ((key-eq key +key-h+)
	   (format t "help~%")
	   (display-help console)
	   'indungeon))))

(defun make-turn (dungeon)
  (incf (dungeon-turns dungeon))
  (draw-level (nth (player-aktlevel (dungeon-player dungeon)) 
		   (dungeon-levels dungeon))))

(defun display-help (console))

(defun do-death (console dungeon)
  'end)

(defun do-win (console dungeon)
  'end)

(defun draw-level (level))

(defun convert-test-level (name str)
  "Convert character string level representation to data structs."
  (let ((lines (split-by str #\Newline))
	 (maxlength 0))
    (dolist (l lines)
      (when (< maxlength (length l)) (setf maxlength (length l))))
    (let ((map_ (make-array (list maxlength (length lines)) :element-type 'character
			   :initial-element #\Space))
	  (monsters ()) (items ())
	  (x 0) (y 0))
      (dolist (l lines)
	(map nil #'(lambda (c)
		     (let ((monsta (find-monster-data (string c) :getter get-monster-char))
			   (item (find-item-data (string c) :getter get-item-char))
			   (feature (find-dungeonfeature-data (string c) :getter get-dungeonfeature-char)))
		       (cond (monsta
			      (format t "monsta ~A at ~A,~A~%" c x y)
			      (pushnew (make-player :name "monsta" :typeid (funcall get-monster-typeid monsta)) monsters)
			      (setf (aref map_ x y) #\.))
			     (item
			      (format t "item ~A at ~A,~A~%" c x y)
			      (pushnew (make-player :name "item" :typeid (funcall get-item-typeid item)) items)
			      (setf (aref map_ x y) #\.))
			     (feature
			      (format t "feature ~A at ~A,~A~%" c x y)
			     (setf (aref map_ x y) c))))
		     (incf x))
	     l)
	(incf y) (setf x 0))
      (make-level :name name :parents nil :childs nil
		  :monsters monsters :items items
		  :map map_))))
