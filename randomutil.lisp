(defun clast (lst)
  (car (last lst)))

(defun filter (lst &optional (test #'identity))
  (reduce 'append (mapcar (lambda (x) (if (funcall test x) (list x) nil)) lst)))

(defun repeater (function x)
  (if (< x 1)
      nil
      (cons function (repeater function (1- x)))))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun range (x min max)
  (and (>= x min)
       (<= x max)))

(defun positions (x lst &key (key #'identity key-supplied-p))
  (let ((a (if key-supplied-p 
	       (position x lst :key key)
	       (position x lst))))
    (if (null a)
	nil
	(cons a (mapcar (lambda (y) (+ a 1 y))
			(positions x (subseq lst (1+ a)) :key key))))))

(defun varer (int char) 
  (let ((z (char-code char)))
    (if (and (> z 96) (< z 123))
        (map-int (lambda (x) (intern (format nil "~A~A" (code-char (- z 32)) x)))
		 int)
	(map-int (lambda (x) (intern (format nil "~A~A" (code-char z) x)))
		 int))))

(defun varbreak (symbol)
  (let ((string (string symbol)))
    (list (char string 0) (parse-integer (subseq string 1)))))

(defun numvec (x &optional (start 0))
  (let ((vec (make-array x)))
    (do ((i start (1+ i))
	 (j 0 (1+ j)))
	((= j x) vec)
      (setf (svref vec j) i))))

(defun num (x &optional (start 0))
  (let ((lst nil))
    (do ((i start (1+ i)))
	((= i (+ start x)) (nreverse lst))
      (push i lst))))

(defun letterer (x)
  (mapcar (lambda (y) (code-char (+ 64 y)))
	(num x 1)))

(defun random-pull (lst)
  (remove (nth (random (length lst)) lst) lst))

(defun my-dotimes (function lst x)
  (if (< x 1)
      lst
      (funcall function (my-dotimes function lst (1- x)))))

(defun puller (x y)
    (my-dotimes #'random-pull (num x 1) (- x y)))

(defun plaus (lst number-desired)
  (mapcar (lambda (x) (nth (1- x) lst))
	  (puller (length lst) number-desired)))

(defun cantzero (x)
  (if (<= x 0)
      1
      x))

(defun random-choice (lst)
  (nth (random (length lst)) lst))
(setf funs '(sin car and not atom))

(defun random-order (lst)
  (mapcar (lambda (x) (nth x lst))
	  (random-order-1 (num (length lst)) nil)))

(defun random-order-1 (lst2 lst3)
  (let ((plaus (plaus lst2 1)))
    (if (null lst2)
	lst3
	(random-order-1 (remove (car plaus) lst2) (append lst3 plaus)))))

(defun rrr (lst x)
  "right-random-round"
  (mapcar (lambda (x y) (+ (floor x) y))
	  lst
	  (rounder (mapcar 'mantissa lst) '() x)))

;(rrr (mapcar 'eval '((/ 3) (/ 5) (/ 4) (/ 6))) 3)

(defun rounder (lst lst2 x)
  (if (null lst)
      lst2
      (let* ((mult (float (apply '+ lst)))
	     (nlst (mapcar (lambda (z) (* z (% x mult))) lst))
	     (c (randomround (car nlst)))
	     (nx (- x c)))
	(rounder (cdr nlst) (append lst2 (list c)) nx))))

(defun mantissa (x)
  (abs (- x (floor x))))

(defun randomround (x)
  (+ (floor x) (if (> (random 1.0) (mantissa x))
		   0
		   1)))

(defun % (x y)
  (if (= y 0)
      0
      (/ x y)))

(defun conser (x)
  (if (atom x)
      (format nil "~A" x)
      (concatenate 'string 
		   (format nil "(~A . " (conser (car x))) 
		   (format nil "~A)" (conser (cdr x))))))

