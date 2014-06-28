(use vector-lib)
(use traversal)
(use data-structures)
(use loops)
(use miscmacros)

(define-record forest goats wolves lions)

(define-record-printer (forest f out)
  (fprintf out "#{Forest ~S ~S ~S}"
           (forest-goats f) (forest-wolves f) (forest-lions f)) )

(define (add-fors a b)
  (make-forest
   (+ (forest-goats a) (forest-goats b))
   (+ (forest-wolves a) (forest-wolves b))
   (+ (forest-lions a) (forest-lions b))))

(define (forest-stable aforest)
  (or
   (and
    (not (eqv? (forest-goats aforest) 0))
    (eqv? (forest-wolves aforest) 0)
    (eqv? (forest-lions aforest) 0))
   (and
    (eqv? (forest-goats aforest) 0)
    (not (eqv? (forest-wolves aforest) 0))
    (eqv? (forest-lions aforest) 0))
   (and
    (eqv? (forest-goats aforest) 0)
    (eqv? (forest-wolves aforest) 0)
    (not (eqv? (forest-lions aforest) 0)))))

(define (forest-invalid aforest)
 (or (< (forest-goats aforest) 0)
     (< (forest-wolves aforest) 0)
     (< (forest-lions aforest) 0)))

(define (meal aforest)
  (vector (add-fors (make-forest -1 -1  1) aforest)
          (add-fors (make-forest -1  1 -1) aforest)
          (add-fors (make-forest  1 -1 -1) aforest)))

(define (for< f1 f2)
  (let ((res #f))
    (if (eqv? (forest-goats f1) (forest-goats f2))
    	(if (eqv? (forest-wolves f1) (forest-wolves f2))
		(if (eqv? (forest-lions f1) (forest-lions f2))
		    (set! res #f)
		(set! res (< (forest-lions f1) (forest-lions f2))))
	(set! res (< (forest-wolves f1) (forest-wolves f2))))
    (set! res (< (forest-goats f1) (forest-goats f2))))
  res))	

(define (qs-fors fors start len)
  (when (> len 1)
  	(let (
	     (p (vector-ref fors (inexact->exact (floor (/ (+ start len) 2)))))
	     (left start)
	     (right (- (+ start len) 1)))
  	  (do-while (<= left right)
    	    (if (for< (vector-ref fors left) p)
      	      (inc left)
	      (if (for< p (vector-ref fors right))
                (dec right)
                (begin
                  (vector-swap! fors left right)
                  (inc left)
                  (dec right))))
          (qs-fors fors start (+(- right start) 1))
          (qs-fors fors left (-(+ start len) left))))))

(define (add-meals new-fors afor i)
       (let ((g (forest-goats afor))
              (w (forest-wolves afor))
              (l (forest-lions afor)))
           (vector-set! new-fors i (make-forest (- g 1) (- w 1) (+ l 1))) 
           (vector-set! new-fors (+ i 1) (make-forest (- g 1) (+ w 1) (- l 1)))
           (vector-set! new-fors (+ i 2) (make-forest (+ g 1) (- w 1) (- l 1)))))

(define (meals forests)
  (let ((new-fors 
          (make-vector (* (vector-length forests) 3)))
         (i 0))
      (do ((i 0 (+ i 1)))
         ((= i (vector-length forests)) i)
           (add-meals new-fors (vector-ref forests i)(* i 3)))
       (sort! new-fors for<)	
      new-fors))

(define (dedup forests)
  (let (
        (uniq-fors (vector))
        (last-for (vector-ref forests 0)))
    (set! uniq-fors (vector-append uniq-fors (vector last-for)))
    (do-times j (vector-length forests)
       (if (or (equal? (vector-ref forests j) last-for) (< j 1))
         #f
         (begin
            (set! last-for (vector-ref forests j))
            (set! uniq-fors (vector-append uniq-fors (vector last-for))))))
    uniq-fors)) 

(define (unique-meals forests)
  (dedup (meals forests)))

(define (devouring-possible forests)
  (and
   (not (vector-empty? forests))   
   (not (vector-any forest-stable forests))))

(define (stable-forests forests)
  (let ((stable-fors (vector)))
    (do-times i (vector-length forests)
      (if (forest-stable (vector-ref forests i))
         (set! stable-fors (vector-append stable-fors (vector (vector-ref forests i))))
         #f))
    stable-fors))

(define (find-stable-forests aforest)
  (let ((forests (vector aforest)))
    (do-while (devouring-possible forests)
      (set! forests (unique-meals forests)))
    (stable-forests forests)))

(define (run)
  (let (
         (g (string->number (list-ref (argv) 1)))
         (w (string->number (list-ref (argv) 2)))
         (l (string->number (list-ref (argv) 3))))
  (print "Stable forests:")
  (print (find-stable-forests (make-forest g w l)))))

(run)