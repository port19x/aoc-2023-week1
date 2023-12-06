(setq text (s-split "\n" (f-read "aoc-input") t)
      times (->> (car text) (s-split-words) (cdr))
      distances (->> (car (cdr text)) (s-split-words) (cdr)))

(defun distance (press time)
  (* press (- time press)))

(defun dt (time)
  (mapcar (lambda (x) (distance x time)) (number-sequence 1 time)))

(defun filterd (time distance)
  (->> (dt time)
       (-filter (lambda (x) (> x distance)))))

; STAR 1
(setq times1 (mapcar 'string-to-number times)
      distances1 (mapcar 'string-to-number distances))

(->> (-zip-lists times1 distances1)
     (mapcar (lambda (x) (apply 'filterd x)))
     (mapcar 'length)
     (apply '*))

; STAR 2
(setq time2 (string-to-number (string-join times))
      distance2 (string-to-number (string-join distances)))

(length (filterd time2 distance2))
