(load "randomutil.lisp")

;;;;Utilities

(defun vowelp (char)
  (if (member (char-code char) '(65 69 73 79 85 97 101 105 111 117))
      t
      nil))

(defun dipthongp (part)
  (if (or (equal "oe" part) (equal "ae" part))
      t
      nil))

(defun consonp (part)
  (if (and (u-i (char part 0))
	   (vowelp (char part 1)))
      t
      nil))

(defun u-i (char)
  (if (member (char-code char) '(117 105 85 73))
      t
      nil))

(defun eqm (char)
  (if (member (char-code char) '(77 109))
      t
      nil))

(defun eqh (char)
  (if (member (char-code char) '(72 104))
      t
      nil))

(defun eqy (char)
  (if (member (char-code char) '(121 89))
      t
      nil))

;;;;Rules

(defun rule-1 (word0 word1)
  "bla alb"
  (if (and (vowelp (char word0 (1- (length word0))))
	   (vowelp (char word1 0)))
      t
      nil))

(defun rule-2 (word0 word1)
  "armam alba"
  (if (and (vowelp (char word0 (- (length word0) 2)))
	   (eqm (char word0 (- (length word0) 1)))
	   (vowelp (char word1 0)))
      t
      nil))

(defun rule-3 (word0 word1)
  "arma iuta"
  (if (and (vowelp (char word0 (1- (length word0))))
	   (u-i (char word1 0))
	   (vowelp (char word1 1)))
      t
      nil))

(defun rule-4 (word0 word1)
  (if (and (vowelp (char word0 (1- (length word0))))
	   (eqh (char word1 0))
	   (or (vowelp (char word1 1))
	       (eqy (char word1 1))))
      t
      nil))

(defun rule-5 (word0 word1)
  "jalai ola"
  (if (and (dipthongp (subseq word0 (- (length word0) 2)
			      (length word0)))
	   (or (vowelp (char word1 0))
	       (eqh (char word1 0))))
      t
      nil))

(defun rule-6 (word0 word1)
  "armam/arma/armia est/es"
  (if (and (or (vowelp (char word0 (1- (length word0))))
	       (dipthongp (subseq word0 (- (length word0) 2)))
	       (eqm (char word0 (1- (length word0)))))
	   (or (equal word1 "est")
	       (equal word1 "es")))
      t
      nil))

(defun rule-7 (word0 word1)
  "armam Iove"
  (ignore-errors (if (and (or (eq (char word1 0) #\i)
			      (eq (char word1 0) #\I))
			  (vowelp (char word1 1)))
		     t
		     nil)))

(defun rule-8 (word0 word1)
  "armae e"
  (if (and (= (length word1) 1)
	   (vowelp (char word1 0))
	   (or (eqm (char word0 (- (length word0) 1)))
	       (vowelp (char word0 (- (length word0) 1)))))
      t
      nil))

(defun rule-8a (word0 word1)
  "armae e"
  (if (and (= (length word1) 1)
	   (vowelp (char word1 0))
	   (dipthongp (subseq word0 (- (length word0) 2))))
      t
      nil))

(defun rule-9 (word0 word1)
  (if (and (eqm (char word0 (1- (length word0))))
	   (eqh (char word1 0)))
      t
      nil))

;;;;Main

(defun words (seq)
  (let ((pos (positions t seq :key (lambda (x) (or (eq #\Space x)
						   (eq #\Tab x))))))
    (mapcar (lambda (x y) (subseq seq x y))
	    (cons 0 (mapcar '1+ pos))
	    (append pos (cons (length seq) nil)))))

(defun encase (str)
  (concatenate 'string "(" str ")"))

(defun group (char)
  (if (or (alpha-char-p char)
	  (eq #\/ char))
      t
      nil))

(defun ender (str)
  (subseq str (1+ (position t str :key #'group :from-end t))))

(defun ender-comp (str)
  (subseq str 0 (1+ (position t str :key #'group :from-end t))))

(defun starter (str)
  (subseq str 0 (position t str :key #'group)))

(defun starter-comp (str)
  (subseq str (position t str :key #'group)))

(defun protocol (str0 str1)
  (let ((ender (ender str0))
	(str2 (ender-comp str0))
	(starter (starter str1))
	(ender2 (ender str1))
	(str3 (starter-comp (ender-comp str1))))
    (cond ((or (= 1 (length str2)) (eq (char str2 0) #\/)
	       (rule-7 str2 str3)) 
	   (concatenate 'string str2 ender " " starter str3 ender2))
	  ((rule-6 str2 str3)
	   (concatenate 'string str2 ender " " starter
			(encase (subseq str3 0 1)) (subseq str3 1)
			ender2))
	  ((rule-8a str2 str3)
	   (concatenate 'string (subseq str2 0 (- (length str2) 2))
			(encase (subseq str2 (- (length str2) 2)))
			ender " " starter str3 ender2))
	  ((rule-8 str2 str3)
	   (concatenate 'string (subseq str2 0 (- (length str2) 1))
			(encase (subseq str2 (- (length str2) 1)))
			ender " " starter str3 ender2))
	  ((rule-5 str2 str3)
	   (concatenate 'string (subseq str2 0 (- (length str2) 2))
			(encase (subseq str2 (- (length str2) 2)
					(length str2)))
			ender " " starter str3 ender2))
	  ((rule-4 str2 str3)
	   (concatenate 'string (subseq str2 0 (- (length str2) 1))
			(encase (subseq str2 (- (length str2) 1)))
			ender " " starter str3 ender2))
	  ((rule-3 str2 str3)
	   (concatenate 'string str2 ender " " starter str3 ender2))
	  ((rule-2 str2 str3)
	   (concatenate 'string (subseq str2 0 (- (length str2) 1))
			(encase (subseq str2 (- (length str2) 1)))
			ender " " starter str3 ender2))
	  ((rule-1 str2 str3)
	   (concatenate 'string (subseq str2 0 (- (length str2) 1))
			(encase (subseq str2 (- (length str2) 1)))
			ender " " starter str3 ender2))
	  ((rule-9 str2 str3)
	   (concatenate 'string (subseq str2 0 (- (length str2) 1))
			(encase (subseq str2 (- (length str2) 1)))
			ender " " starter str3 ender2))
	  (t (concatenate 'string str2 ender " "
			  starter str3 ender2)))))

(defun gnomep (str)
  (if (and (eq #\( (elt str 0))
	   (eq #\) (elt str 2)))
      t
      nil))

(gnomep "(oe)")

(defun supergnomep (str)
  (if (and (eq #\( (elt str 0))
	   (eq #\) (elt str 3)))
      t
      nil))

(defun gnome-at (str int)
  (gnomep (subseq str int (+ int 3))))

(defun supergnome-at (str int)
  (supergnomep (subseq str int (+ int 4))))
      
(supergnome-at "bla (oe)" 3)

(defun gnome-counter (str)
  (count 't (map-int (lambda (x) (gnome-at str x))
		     (- (length str) 2))))

(defun supergnome-counter (str) ;wrong
  (count 't (map-int (lambda (x) (supergnome-at str x))
		     (- (length str) 3))))

(defun letter-counter (str)
  (- (count t str :key 'alpha-char-p) (gnome-counter str)
     (* 2 (supergnome-counter str))))

(defun add-line-numbers (corpus)
  (with-open-file (toy corpus)
    (do ((i 1 (1+ i)))
	((= i 11) nil)
      (format t "~4,A ~A~%" i (read-line toy t nil t)))))

(defun depurar (str)
  (remove 0 (words str) :key 'length))

(defun nothing (str)
  (or (null str) (string-equal "" str)))

(defun process (corpus)
  (let ((len 0))
    (with-open-file (toy corpus)
      (do ((i (read-line toy nil nil) (read-line toy nil nil)))
	  ((nothing i) len)
	(incf len)))
    (with-open-file (toy corpus)
      (do ((i 1 (1+ i))
	   (count 0))
	  ((> i len) nil)
	(let* ((str (read-line toy))
	       (str2 (reduce #'protocol (depurar str)))
	       (start (if (eq (char str 0) #\Tab)
			  "	"
			  "")))
	  (incf count (letter-counter str2))
	  (format t "~4@A ~6@A ~A~%" i count
		  (concatenate 'string start str2)))))))

(defun count-letters (corpus)
  (let ((len 1))
    (with-open-file (toy corpus)
      (do ((i (read-line toy nil nil) (read-line toy nil nil)))
	  ((nothing i) len)
	(incf len)))
    (with-open-file (toy corpus)
      (do ((i 1 (1+ i))
	   (count 0))
	  ((= i len) count)
	(let* ((str (read-line toy))
	       (str2 str)
	       (start (if (eq (char str 0) #\Tab)
			  "	"
			  "")))
	  (incf count (letter-counter str2))
	  (format t "~4@A ~6@A ~A~%" i count
		  (concatenate 'string start str2)))))))

(process (car *args*))
