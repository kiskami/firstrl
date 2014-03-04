;;; utils.lisp

(in-package #:firstrl)

(defun split-by (string &optional (sepchar #\Space))
  "Returns a list of substrings of string
divided by ONE separator char each.
http://cl-cookbook.sourceforge.net/strings.html"
  (loop for i = 0 then (1+ j)
     as j = (or (position sepchar string :start i))
     collect (subseq string i j)
     while j))
