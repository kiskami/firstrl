;;; msgwindow.lisp
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

(defparameter *msgwindow* ())

(defun add-msg (msg)
;  (display-text-wrapped console 1 27 (- (consoledata-w console) 2) 2 msg)
  (when (> (length *msgwindow*) +MSGWIND-H+) (pop *msgwindow*))
  (setf *msgwindow* (nconc *msgwindow* (list msg))))

(defun display-msg-window (console x y)
  (dotimes (i (length *msgwindow*))
    (display-text console x (+ i y) (nth i *msgwindow*))))
