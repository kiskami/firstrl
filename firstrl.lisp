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
				    :levels (gen-levels player))))
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
  (display-text console 10 10 20 50 +INTRO-TEXT+ 'center)
  (update-console console)
  (wait-for-keypress +key-space+ console)
  (clear-console console)
  (update-console console)
  'chargen)

(defun do-chargen (console)
  (make-player 
   :name "name"
   :gender 'm
   :role 'monk
   :alignment 'chaothic-good
   :deity 'zumba
   :inventory nil
   :armor nil
   :weapons nil
   :tools nil
   :hp 100))

(defun gen-levels (player)
  nil)

(defun do-update-dungeon (console dungeon)
  (let ((key (wait-for-any-key console)))
    (cond ((key-eq key +key-q+)
	   'end)
	  (t
	   (format t "*")
	   'indungeon))))

(defun do-death (console dungeon)
  'end)

(defun do-win (console dungeon)
  'end)
