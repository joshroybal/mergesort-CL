(setq *random-state* (make-random-state t))

(defun random-integer () (random 2147483647))
(defun random-real () (random 1.0))

(defun random-list (n fn)
  (do ((lis nil (cons (funcall fn) lis))
       (i 0 (+ i 1)))
    ((equal i n) lis)))

(defun extract-runs (lis fn)
  (do* ((x lis (cdr x))
        (curr (car x) next)
        (next (cadr x) (cadr x))
        (current-run nil)
        (run-list nil))
    ((null curr) (mapcar #'nreverse run-list))
    (cond ((null next)
           (if (null current-run)
             (setf run-list (cons (list curr) run-list))
             (setf run-list (cons (cons curr current-run) run-list))))
          ((funcall fn curr next)
           (setf current-run (cons curr current-run)))
          (t
            (setf run-list (cons (cons curr current-run) run-list))
            (setf current-run nil)))))

(defun merge-lists (run-1 run-2 fn)
  (do* ((x run-1)
        (y run-2)
        (z nil))
    ((and (null x) (null y)) (nreverse z))
    (cond ((null x)
           (setf z (cons (first y) z))
           (setf y (cdr y)))
          ((null y)
           (setf z (cons (first x) z))
           (setf x (cdr x)))
          ((funcall fn (first x) (first y))
           (setf z (cons (first x) z))
           (setf x (cdr x)))
          (t
            (setf z (cons (first y) z))
            (setf y (cdr y))))))

(defun merge-runs (run-list fn)
  (do ((in run-list (cddr in))
       (out nil (cons (merge-lists (first in) (second in) fn) out)))
    ((null in) out)))

(defun merge-sort (lis fn)
  (do* ((in (extract-runs lis fn) out)
        (out in (merge-runs in fn)))
    ((equal (length out) 1) (car out))))

(defun sorted-p (lis fn)
  (do ((x lis (cdr x)))
    ((null (cdr x)) t)
    (if (not (funcall fn (first x) (second x))) (return nil))))

(defun elapsed-time (t1 t2)
  (float (/ (- t2 t1) internal-time-units-per-second)))

(defun read-record (s n) 
  (let ((*record* (make-array 128
                              :element-type 'character
                              :initial-element #\Space)))
    (file-position s (* 128 (- n 1)))
    (read-sequence *record* s) (string-trim " " *record*)))

(defun random-records (n)
  (let ((infile "/home/slacker/dat/data.dat") (NORECS 405995))
    (with-open-file (s infile
                       :element-type 'character
                       :direction :input)
      (do* ((cnt 0 (+ cnt 1))
            (recno (+ (random NORECS) 1) (+ (random NORECS) 1))
            (record (read-record s recno) (read-record s recno))
            (record-list nil (cons record record-list)))
        ((equal cnt n) (close s) record-list)))))
