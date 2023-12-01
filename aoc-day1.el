(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(progn
  (defun day1-1 (x)
    (->> x
         (mapcar (lambda (x) (s-replace-regexp "[a-z]" "" x)))
         (mapcar (lambda (x) (mapcar (lambda (y) (char-to-string y)) x)))
         (mapcar (lambda (x) (list (car x) (car (last x)))))
         (mapcar (lambda (x) (apply 'concat x)))
         (mapcar 'string-to-number)
         (apply '+)))
  (day1-1 (s-split "\n" (f-read "aoc-input") t)))

(progn
  (defun day1-2 (x)
    (->> x
         (mapcar (lambda (x) (s-replace "zero" "z0o" x)))
         (mapcar (lambda (x) (s-replace "one" "o1e" x)))
         (mapcar (lambda (x) (s-replace "two" "t2o" x)))
         (mapcar (lambda (x) (s-replace "three" "t3e" x)))
         (mapcar (lambda (x) (s-replace "four" "f4r" x)))
         (mapcar (lambda (x) (s-replace "five" "f5e" x)))
         (mapcar (lambda (x) (s-replace "six" "s6x" x)))
         (mapcar (lambda (x) (s-replace "seven" "s7n" x)))
         (mapcar (lambda (x) (s-replace "eight" "e8t" x)))
         (mapcar (lambda (x) (s-replace "nine" "n9e" x)))
         (day1-1)))
  (day1-2 (s-split "\n" (f-read "aoc-input") t)))
