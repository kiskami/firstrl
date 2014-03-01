;;; utils.lisp

(in-package #:firstrl)

(defun split-by-white-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them.
http://cl-cookbook.sourceforge.net/strings.html"
    (loop for i = 0 then (1+ j)
       as j = (or (position #\Space string :start i) 
		  (position #\Newline string :start i))
       collect (subseq string i j)
       while j))
