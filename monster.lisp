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
	(dolist (genparam mp)
	  (setf res (append res
			    (apply #'gen-monsters* 
				   leveldata map features items
				   genparam))))))
    res))

(defun gen-monsters* (leveldata map features items &key typeid (cnt 1))
  (let ((res ()))
    (dotimes (i cnt res)
      (let ((md (gethash typeid *monsterdata*))
	    (kord (find-kord-for-monster leveldata map features items res))
	    )
	(cond (md
	       (setf res (append res
				 (make-lifeform
				  :name (monsterdata-name md)
				  :typeid typeid
				  :x (car kord) :y (cdr kord)
				  :state 'alive
				  :thinkfunc #'monster-think
				  :turns 0
				  :role (monsterdata-rel md)
				  :hp (monsterdata-hp md)
				  :maxhp (monsterdata-hp md)
				  :att (monsterdata-att md)
				  :def (monsterdata-def md)
				  :spe (monsterdata-spe md)
				  :xp (monsterdata-xp md)
				  :alignment (monsterdata-rel md))))
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
      ((and (not (is-floor map x y))
	    (not (is-feature-at x y fea))
	    (not (is-item-at x y ite))
	    (not (in-monsterlist x y ml))
	    ) (cons x y))
    ;empty
    ))

(defun is-feature-at (x y fealist))

(defun is-item-at (x y itemlist))

(defun in-monsterlist (x y lst)
  (dolist (m lst nil)
    (if (and (= x (lifeform-x m))
	     (= y (lifeform-y m)))
	(return-from in-monsterlist t))))

(defun monster-think (msg)
  (declare (ignore msg)))
