(defpackage #:magic-forest
  (:nicknames :mf)
  (:use #:cl))

(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

(in-package #:magic-forest)
(export '(run))

(defstruct forest (goats 0) (wolves 0) (lions 0))

(defun add-fors (a b)
  (make-forest 
   :goats (+ (forest-goats a) (forest-goats b)) 
   :wolves (+ (forest-wolves a) (forest-wolves b))
   :lions (+ (forest-lions a) (forest-lions b))))

(defun forest-stable (aforest)
  (or 
   (and 
    (not (eql (forest-goats aforest) 0))
    (eql (forest-wolves aforest) 0)
    (eql (forest-lions aforest) 0))
   (and 
    (eql (forest-goats aforest) 0)
    (not (eql (forest-wolves aforest) 0))
    (eql (forest-lions aforest) 0))
   (and 
    (eql (forest-goats aforest) 0)
    (eql (forest-wolves aforest) 0)
    (not (eql (forest-lions aforest) 0)))))

(defun forest-invalid (aforest)
 (or (< (forest-goats aforest) 0)
     (< (forest-wolves aforest) 0)
     (< (forest-lions aforest) 0)))

(defun meal (aforest)
  (vector (add-fors (make-forest :goats -1 :wolves -1 :lions 1) aforest)
          (add-fors (make-forest :goats -1 :wolves 1 :lions -1) aforest)
          (add-fors (make-forest :goats 1 :wolves -1 :lions -1) aforest)))

(defun for< (f1 f2)
  (let ((res nil))
    (if (eql (forest-goats f1) (forest-goats f2))
	(if (eql (forest-wolves f1) (forest-wolves f2))
	    (if (eql (forest-lions f1) (forest-lions f2))
		(setf res nil)
	      (setf res (< (forest-lions f1) (forest-lions f2))))
	  (setf res (< (forest-wolves f1) (forest-wolves f2))))
      (setf res (< (forest-goats f1) (forest-goats f2))))
    res))	     

(defun for-equal (f1 f2)
  (and (eql (forest-goats f1) (forest-goats f2))
       (eql (forest-wolves f1) (forest-wolves f2))
       (eql (forest-lions f1) (forest-lions f2))))

(defun join-fors (fors)
  (reduce (lambda (a b)
            (concatenate 'vector a b))
	  fors))

(defun meals (forests)
  (stable-sort
   (join-fors
    (map '(vector) #'meal forests))
  #'for<))

(defun dedup (forests)
  (let (
	(uniq-fors (make-array (length forests) :fill-pointer 0 :element-type 'forest))
	(last-for (aref forests 0)))
    (vector-push last-for uniq-fors)
    (loop for f across forests do
	  (if (for-equal f last-for)
	      nil
	    (progn
	      (vector-push f uniq-fors)
	      (setf last-for f))))
    uniq-fors))

(defun unique-meals (forests)
  (dedup (meals forests)))

(defun devouring-possible (forests)
  (and 
   (not (< (first (array-dimensions forests)) 0))
   (notany #'forest-stable forests)))
	
(defun stable-forests (forests)
  (remove-if (complement #'forest-stable) forests))

(defun find-stable-forests (forest)
  (let ((forests (vector forest)))
    (loop while (devouring-possible forests)
      do (setf forests (unique-meals forests))
      finally (print (stable-forests forests)))))

(defun run (g w l)
  (print (find-stable-forests (make-forest :goats g :wolves w :lions l))))

