(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defun heading? (x)
  (not (or (string-equal x "Game")
      (string-equal x "blue")
      (string-equal x "green")
      (string-equal x "red"))))

(defun both (x)
    (->> x
         (mapcar (lambda (x) (s-split-words x)))
         (mapcar (lambda (x) (-partition 2 x)))
         (mapcar (lambda (x) (-group-by (lambda (y) (nth 1 y)) x)))
         (mapcar (lambda (x) (mapcar (lambda (y) (-flatten y)) x)))
         (mapcar (lambda (x) (mapcar (lambda (y) (sort y #'string<)) x)))
         (mapcar (lambda (x) (mapcar (lambda (y) (reverse y)) x)))
         (mapcar (lambda (x) (sort x (lambda (y z) (string< (car y) (car z))))))
         ; game, blue, green, red
         (mapcar (lambda (x) (mapcar (-partial '-filter 'heading?) x)))
         (mapcar (lambda (x) (mapcar (lambda (y) (mapcar 'string-to-number y)) x)))
         ))

(progn
  (defun day2-1 (x)
    (->> x
         both
         (mapcar (lambda (x) (mapcar (lambda (y) (apply 'max y)) x)))
         ; only 14 blue cubes, 13 green cubes, and 12 red cubes?
         (-filter (lambda (x) (<= (nth 1 x) 14)))
         (-filter (lambda (x) (<= (nth 2 x) 13)))
         (-filter (lambda (x) (<= (nth 3 x) 12)))
         (mapcar 'car)
         (apply '+)
         ))
  (day2-1 (s-split "\n" (f-read "aoc-input") t)))

(progn
  (defun day2-2 (x)
    (->> x
         both
         (mapcar 'cdr)
         (mapcar (lambda (x) (mapcar (lambda (y) (apply 'max y)) x)))
         (mapcar (-partial 'apply '*))
         (apply '+)
         ))
  (day2-2 (s-split "\n" (f-read "aoc-input") t)))
