;;; monster.lisp
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

(defun gen-monsters (leveldata map features items)
  (let ((res ()))
    (dolist (mp (leveldata-monsters leveldata))
      (when mp
	(setf res (append res
			  (apply #'gen-monsters* 
				 :leveldata leveldata 
				 :map map 
				 :features features 
				 :items items
				 mp)))))
    res))

(defun gen-monsters* (&key leveldata map features items typeid (cnt 1) (sickprob 0))
  (let ((res ()))
    (dotimes (i cnt res)
      (let ((md (gethash typeid *monsterdata*))
	    (kord (find-kord-for-monster leveldata map features items res))
	    (sick (<= (get-rnd-number 0 100) sickprob))
	    )
	(cond (md
	       (setf res (append res
				 (list (make-lifeform
				   :name (monsterdata-name md)
				   :typeid typeid
				   :x (car kord) :y (cdr kord)
				   :state (if sick 'sick 'alive)
				   :thinkfunc #'monster-think
				   :turns 0
				   :role (monsterdata-rel md)
				   :hp (monsterdata-hp md)
				   :maxhp (monsterdata-hp md)
				   :att (monsterdata-att md)
				   :def (monsterdata-def md)
				   :spe (monsterdata-spe md)
				   :xp (monsterdata-xp md)
				   :alignment (monsterdata-rel md)))))
	       )
	      (t
	       (error "Monsterdata not found.")))
	)
      ))
  )

(defun find-kord-for-monster (leveldata map fea ite ml)
  "lm - monsterlist so far by gen-monsters*"
  (do ((x 0 (get-rnd-number 1 (1- (leveldata-w leveldata))))
       (y 0 (get-rnd-number 1 (1- (leveldata-h leveldata)))))
      ((and (is-floor map x y)
	    (not (is-object-at x y fea)) ; feature there?
	    (not (is-object-at x y ite)) ; item there?
	    (not (is-object-at x y ml))	; monster there?
	    ) (cons x y))
    ;empty
    ))

(defun monster-think (msg)
  (declare (ignore msg)))

(defun damage-monster (level monsta dmg)
  (cond ((>= dmg (lifeform-hp monsta))
	 (setf (lifeform-hp monsta) 0
	       (lifeform-state monsta) 
	       (if (equal (lifeform-state monsta) 'sick) 'sick-dead 'dead))
	 (deadmonster-to-corpse level monsta)
	 (add-msg (format nil "The ~A dies." (lifeform-name monsta)))
 )
	(t
	 (decf (lifeform-hp monsta) dmg)
	 (when (<= (lifeform-hp monsta) (/ (lifeform-maxhp monsta) 3))
	   (add-msg (format nil "The ~A is heavily wounded." 
				    (lifeform-name monsta))))
	 )
	)
  )

(defun monsta-alive (monsta)
  (not (or (equal (lifeform-state monsta) 'dead) (equal (lifeform-state monsta) 'sick-dead))))

(defun deadmonster-to-corpse (level monsta)
  (setf 
   (level-monsters level) 
   (delete monsta (level-monsters level))
   (level-items level) 
   (nconc (level-items level)
	  (list (make-objectwithrole
		 :name (format nil "~A corpse" (lifeform-name monsta))
		 :typeid (if (equal 'sick-dead (lifeform-state monsta))
			     "%sc" "%c")
		 :aktlevel 0
		 :x (lifeform-x monsta)
		 :y (lifeform-y monsta)
		 :state 'dead
		 :role "corpse"
		 :xp (floor (/ (lifeform-xp monsta) 2)))))))
