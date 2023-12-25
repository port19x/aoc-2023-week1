(setq text (s-split "\n" (f-read "aoc-input") t)
      directions (-drop-last 1 (cdr (s-split "" (car text))))
      pairs (mapcar 's-split-words (cdr text))
      i 0
      element "AAA")

;(move "L" '("AAA" "BBB" "CCC")) => "BBB"
;(move "R" '("AAA" "BBB" "CCC")) => "CCC"
(defun move (x node)
  (if (string= x "R")
      (caddr node)
    (cadr node)))

;(non-alist-get "ZZZ" pairs) => ("ZZZ" "ZZZ" "ZZZ")
(defun non-alist-get (x y)
  (car (-filter (lambda (z) (string= x (car z))) y)))

(while (not (string= element "ZZZ"))
  (let ((node (non-alist-get element pairs)))
    (setq element (move (nth (mod i (length directions)) directions) node)
	  i (1+ i))))
(identity i)
