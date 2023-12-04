(defun matches (a b)
  (->> b
       (-filter (lambda (x) (member x a)))
       (-non-nil)
       (length)))

(defun both (x)
  (->> x
       (mapcar (lambda (x) (s-split " " x t)))
       (mapcar (lambda (x) (-drop 2 x)))
       (mapcar (lambda (x) (-split-on "|" x)))
       (mapcar (lambda (x) (mapcar (lambda (y) (mapcar (lambda (z) (string-to-number z)) y)) x)))
       (mapcar (lambda (x) (apply 'matches x)))))

(progn
  (defun day4-1 (x)
    (->> x
         (both)
         (mapcar (lambda (x) (- x 1)))
         (mapcar (lambda (x) (if (>= x 0) (expt 2 x) 0)))
         (apply '+)))
  (day4-1 (s-lines (f-read "aoc-input"))))

(progn ;; day4-2
  (setq day4-2 (both (s-lines (f-read "aoc-input")))
        masterlist (-repeat (length day4-2) 1)
        i 0)
  (while (< i (length day4-2))
    (let* ((a (+ i 1))
           (b (nth i day4-2))
           (c (- (length day4-2) a b))
           (d (append (-repeat a 0) (-repeat b (nth i masterlist)) (-repeat c 0))))
      (progn
        (setq masterlist (mapcar (-partial 'apply '+) (-zip-lists d masterlist)))
        (setq i (+ i 1)))))
  (-sum masterlist))
