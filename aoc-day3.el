(setq file (->> "aoc-input" (f-read) (s-lines))
      field (->> file
		 (mapcar (lambda (x) (s-split "" x t)))
		 (mapcar (lambda (x) (-filter (lambda (y) (not (s-blank? y))) x))))
      width (->> file (car) (length))
      length (->> file length))

;(check 0 2 field)
;(check 0 0 field)
(defun nmlz (x) (if x x "."))
(defun check (i j field)
  (not (and (s-matches? "[0-9.]" (nmlz (nth (- j 1) (nth (+ i 1) field))))
	    (s-matches? "[0-9.]" (nmlz (nth (- j 1) (nth (+ i 0) field))))
	    (s-matches? "[0-9.]" (nmlz (nth (- j 1) (nth (- i 1) field))))
	    (s-matches? "[0-9.]" (nmlz (nth (+ j 0) (nth (+ i 1) field))))
	    (s-matches? "[0-9.]" (nmlz (nth (+ j 0) (nth (- i 1) field))))
	    (s-matches? "[0-9.]" (nmlz (nth (+ j 1) (nth (+ i 1) field))))
	    (s-matches? "[0-9.]" (nmlz (nth (+ j 1) (nth (+ i 0) field))))
	    (s-matches? "[0-9.]" (nmlz (nth (+ j 1) (nth (- i 1) field)))))))

(progn
  (setq i 0 j 0 sum 0 partnumber "" applicable nil)
  (while (<= i length)
    (while (<= j width)
      (if (s-numeric? (nmlz (nth j (nth i field))))
	  (setq partnumber (s-join "" (list partnumber (nth j (nth i field))))
		applicable (or applicable (check i j field)))
	(unless (s-blank? partnumber)
	  (setq sum (if applicable (+ sum (string-to-number partnumber)) sum)
		partnumber ""
		applicable nil)))
      (setq j (1+ j)))
    (setq i (1+ i)
	  j 0))
  (identity (list i j sum partnumber applicable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(readright 5 (nth 0 field))
;(readright 4 (nth 0 field))
(defun readright (i row)
  (if (>= i (length row)) 'nil
    (if (not (s-numeric? (nmlz (nth i row)))) 'nil
	(s-join "" (list (nth i row) (readright (1+ i) row))))))

;(seekleft 2 (nth 0 field))
;(seekleft 4 (nth 0 field))
(defun seekleft (i row)
  (if (< i 0) (readright (1+ i) row)
    (if (s-numeric? (nmlz (nth i row)))
	(seekleft (1- i) row)
      (readright (1+ i) row))))

;(branchbuild 8 5 field)
;(branchbuild 1 3 field)
(defun branchbuild (i j field)
  (-non-nil
   (list
    (seekleft (1- j) (nth i field))         ;left
    (readright (1+ j) (nth i field))        ;right
    (seekleft (1- j) (nth (1- i) field))    ;top
    (unless (s-numeric? (nmlz (nth j (nth (1- i) field))))
      (readright (1+ j) (nth (1- i) field)))
    (seekleft (1- j) (nth (1+ i) field))    ;bottom
    (unless (s-numeric? (nmlz (nth j (nth (1+ i) field))))
      (readright (1+ j) (nth (1+ i) field))))))

(defun starsplode (i j field)
  (if (length= (branchbuild i j field) 2)
      (mapcar 'string-to-number (branchbuild i j field))
    '(0 0)))

(progn
  (setq i 0 j 0 sum 0)
  (while (< i length)
    (while (< j width)
      (when (string= "*" (nth j (nth i field)))
	(setq sum (+ sum (apply '* (starsplode i j field)))))
      (setq j (1+ j)))
    (setq i (1+ i) j 0))
  (identity (list i j sum b1 b2)))
