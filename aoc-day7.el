(setq cardranks '(("2" . 2) ("3" . 3) ("4" . 4) ("5" . 5) ("6" . 6)
		  ("7" . 7) ("8" . 8) ("9" . 9) ("T" . 10)
		  ("J" . 11) ("Q" . 12) ("K" . 13) ("A" . 14)))

;(list= '(2 1 1 1) '(3 1 1)) => nil
;(list= '(3 2) '(3 2)) => t
(defun list= (x y)
  (->> (-zip x y)
       (mapcar (lambda (x) (= (car x) (cdr x))))
       (-every 'identity)))

;(parsehand "32T3K") => (3 2 10 3 13)
(defun parsehand (x)
  "Convert hand to numeric list representation"
  (mapcar (lambda (x) (alist-get x cardranks 0 nil 'string-equal)) (s-split "" x t)))

;(r2> '(5 5 5 5 3) '(5 5 5 5 4)) => nil
;(r2> '(5 5 6 5 3) '(5 5 5 5 4)) => t
(defun r2> (x y)
  "Recursive High Card Alorithm"
  (if (= (car x) (car y))
      (r2> (cdr x) (cdr y))
    (> (car x) (car y))))

;(r1> '(5 4 5 4 1) '(3 3 3 2 2)) => nil
;(r1> '(13 13 6 7 7) '(3 2 10 3 13)) => t
(defun r1> (x y)
  "Full Poker Hand Ranking Algorithm"
  (let ((sx (sort (mapcar 'cdr (-frequencies x)) '>))
	(sy (sort (mapcar 'cdr (-frequencies y)) '>)))
    (if (list= sx sy) (r2> x y) (r2> sx sy))))

(->> (f-read "aoc-input")
     (s-lines)
     (mapcar (lambda (x) (s-split " " x)))
     (mapcar (lambda (x) (list (parsehand (nth 0 x)) (string-to-number (nth 1 x)))))
     (-sort (lambda (x y) (r1> (car x) (car y))))
     (mapcar 'cdr)
     (-flatten)
     (reverse)
     ((lambda (x) (-zip-lists (number-sequence 1 (length x)) x)))
     (mapcar (lambda (x) (apply '* x)))
     (-sum))
