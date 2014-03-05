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

(defparameter get-monster-char #'first)
(defparameter get-monster-typeid #'second)
(defparameter get-item-char #'first)
(defparameter get-item-typeid #'second)
(defparameter get-dungeonfeature-char #'first)
(defparameter get-dungeonfeature-typeid #'second)

(defun find-monster-data (id &key (getter get-monster-typeid))
  (find-if #'(lambda (r) (equal id r)) +MONSTERS+ :key getter))

(defun find-item-data (id &key (getter get-item-typeid))
  (find-if #'(lambda (r) (equal id r)) +ITEMS+ :key getter))

(defun find-dungeonfeature-data (id &key (getter get-dungeonfeature-typeid))
  (find-if #'(lambda (r) (equal id r)) +DUNGEONFEATURES+ :key getter))

(defun find-dungeonfeatures-in-level (level typeid)
  (remove-if #'(lambda (f) (equal typeid f)) (level-features level) :key #'object-typeid))

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
		     (let ((monsta (find-monster-data (string c) :getter get-monster-char))
			   (item (find-item-data (string c) :getter get-item-char))
			   (feature (find-dungeonfeature-data (string c) :getter get-dungeonfeature-char)))
		       (cond (monsta
			      (format t "monsta ~A at ~A,~A~%" c x y)
			      (pushnew (make-lifeform :name "monsta" 
						      :typeid (funcall get-monster-typeid monsta) 
						      :x x :y y) monsters)
			      (setf (aref map_ x y) #\.))
			     (item
			      (format t "item ~A at ~A,~A~%" c x y)
			      (pushnew (make-object :name "item" 
						    :typeid (funcall get-item-typeid item) 
						    :x x :y y) items)
			      (setf (aref map_ x y) #\.))
			     (feature
			      (format t "feature ~A at ~A,~A~%" c x y)
			      (pushnew (make-object :name "feature" 
						    :typeid (funcall get-dungeonfeature-typeid feature) 
						    :x x :y y) features)
			     (setf (aref map_ x y) c))))
		     (incf x))
	     l)
	(incf y) (setf x 0))
      (make-level :name name :parents nil :childs nil
		  :monsters monsters :items items
		  :map map_))))
