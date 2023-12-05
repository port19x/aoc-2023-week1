;;; -*- lexical-binding: t; -*-
; First idea: associative lists, not viable due to extreme sequence width
; Second idea: partially applied addition functions, not viable due to bad support
; Third idea: Work backwards, unviable due to passthrough effect
; Fourth idea: Just first idea but let it run
; Fifth idea: Rewrite
(setq text (s-lines (f-read "aoc-input"))
      massage (->> text (-partition-by 's-blank?) (mapcar (lambda (x) (remove "" x))) (-non-nil))
      seeds (->> massage (car) (car) (s-split-words) (cdr) (mapcar 'string-to-number))
      maps (->> massage cdr (mapcar 'cdr)
		(mapcar (lambda (x) (mapcar (lambda (y) (s-split-words y)) x)))
		(mapcar (lambda (x) (mapcar (lambda (y) (mapcar 'string-to-number y)) x)))))

;(maptodelta '(50 98 2))
(defun maptodelta (x)
  (let* ((dest (nth 0 x))
	 (src (nth 1 x))
	 (rlen (nth 2 x))
	 (delta (- dest src))
	 (lower src)
	 (upper (+ src (- rlen 1))))
    (list lower upper delta)))

;(delta? 50 (maptodelta '(50 98 2)))
;(delta? 98 (maptodelta '(50 98 2)))
(defun delta? (x list)
  (let* ((lower (nth 0 list))
	 (upper (nth 1 list)))
    (and (>= x lower) (<= x upper))))

(setq maps (mapcar (lambda (x) (mapcar 'maptodelta x)) maps))

;(deltaapply 50 (car maps))
;(deltaapply 30 (car maps))
(defun deltaapply (x lists)
  (if lists
      (if (delta? x (car lists))
	  (+ x (nth 2 (car lists)))
	(deltaapply x (cdr lists)))
    x))

;(recurlists 79 maps)
(defun recurlists (seed listss)
  (if listss
      (recurlists (deltaapply seed (car listss)) (cdr listss))
    seed))

(-min (mapcar (lambda (x) (recurlists x maps)) seeds))
; STAR 1 FINISH

(setq seedranges (->> seeds
		      (-partition 2)
		      (mapcar (lambda (x) (list (nth 0 x) (- (nth 1 x) 1))))
		      (mapcar (lambda (x) (list (nth 0 x) (+ (nth 0 x) (nth 1 x)))))
		      ((lambda (x) (sort x (lambda (y z) (< (car y) (car z)))))))
      minimum most-positive-fixnum
      i (car (car seedranges))
      localtimer 0
      highgear 10000)

;Temporarily give 8 Gig Ram: (setq gc-cons-threshold (* 8 1024 1024 1024))
;Back to sanity: (setq gc-cons-threshold (* 8 1024 1024))

;22 seconds without gc btw
(benchmark-run 1
  (while seedranges
    (progn
      (when (>= i (car (cdr (car seedranges))))
	(setq seedranges (cdr seedranges))
	(unless (not seedranges)
	  (when (< i (car (car seedranges)))
	    (setq i (car (car seedranges))))))
      (when (< (recurlists i maps) minimum)
	(setq minimum (recurlists i maps)
	      i (- i highgear)
	      localtimer highgear))
      (if (= localtimer 0)
	  (setq i (+ i highgear))
	(progn (setq i (+ i 1)
		     localtimer (- localtimer 1)))))))
(identity minimum)
