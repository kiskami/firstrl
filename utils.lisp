;;; utils.lisp
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

(defun split-by (string &optional (sepchar #\Space))
  "Returns a list of substrings of string
divided by ONE separator char each.
http://cl-cookbook.sourceforge.net/strings.html"
  (loop for i = 0 then (1+ j)
     as j = (or (position sepchar string :start i))
     collect (subseq string i j)
     while j))

(defun find-dungeonfeatures-in-level (level typeid)
  (remove-if-not #'(lambda (f) (equal typeid f)) (level-features level) :key #'object-typeid))

(defun get-player-level (dungeon)
  (nth (lifeform-aktlevel (dungeon-player dungeon)) (dungeon-levels dungeon)))

(defun is-object-at (x y lst)
  (dolist (o lst nil)
    (if (and (= x (object-x o))
	     (= y (object-y o)))
	(return-from is-object-at o))))

(defun is-a-monster (lifeform)
  (gethash (lifeform-typeid lifeform) *monsterdata*))

(defun is-an-item (object)
  (gethash (object-typeid object) *itemdata*))

(defun is-a-dungeonfeature (object)
  (gethash (object-typeid object) *dungeonfeaturedata*))

(defun convert-test-level (name str)
  "Convert character string level representation to data structs."
  (let ((lines (split-by str #\Newline))
	 (maxlength 0))
    (dolist (l lines)
      (when (< maxlength (length l)) (setf maxlength (length l))))
    (let ((map_ (make-array (list maxlength (length lines)) :element-type 'character
			   :initial-element #\Space))
	  (monsters ()) (items ()) (features ())
	  (x 0) (y 0))
      (dolist (l lines)
	(map nil #'(lambda (c)
		     (let ((monsta (gethash (string c) *monsterdata*))
			   (item (gethash (string c) *itemdata*))
			   (feature (gethash (string c) *dungeonfeaturedata*)))
		       (cond (monsta
;			      (format t "monsta ~A at ~A,~A~%" c x y)
			      (pushnew (make-lifeform :name "monsta" 
						      :typeid (string c) 
						      :x x :y y) monsters)
			      (setf (aref map_ x y) #\.))
			     (item
;			      (format t "item ~A at ~A,~A~%" c x y)
			      (pushnew (make-object :name "item" 
						    :typeid (string c) 
						    :x x :y y) items)
			      (setf (aref map_ x y) #\.))
			     (feature
;			      (format t "feature ~A at ~A,~A~%" c x y)
			      (pushnew (make-object :name "feature"
						    :typeid (string c) 
						    :x x :y y) features)
			      (setf (aref map_ x y) c))))
		     (incf x))
	     l)
	(incf y) (setf x 0))
      (make-level :name name :parents nil :childs nil
		  :monsters monsters :items items :features features
		  :map map_))))

(defun get-rnd-number (min max)
  (+ min (random (1+ (- max min)) *RNDSTATE*)))
