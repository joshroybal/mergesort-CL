#!/usr/bin/clisp

(load "merge-sort-module")

(defvar LIM)
(defvar n)
(defvar integers)
(defvar sorted-integers)
(defvar reals)
(defvar sorted-reals)
(defvar records)
(defvar sorted-records)
(defvar t1)
(defvar t2)
(defvar len)

(setf LIM 50)
(setf n (parse-integer (car *args*)))
;;; ascending integers
(format t "~&integers")
(setf integers (random-list n #'random-integer))
(cond ((<= n LIM) (format t "~&~S" integers)))
(format t "~&~S" (sorted-p integers #'<=))
(setf t1 (get-internal-real-time))
(setf sorted-integers (merge-sort integers #'<=))
(setf t2 (get-internal-real-time))
(format t "~&ascending integers")
(cond ((<= n LIM) (format t "~&~S" sorted-integers)))
(format t "~&~S" (sorted-p sorted-integers #'<=))
(format t "~&elapsed time = ~f" (elapsed-time t1 t2))
;;; descending integers
(setf integers (random-list n #'random-integer))
(cond ((<= n LIM) (format t "~&~S" integers)))
(format t "~&~S" (sorted-p integers #'>=))
(setf t1 (get-internal-real-time))
(setf sorted-integers (merge-sort integers #'>=))
(setf t2 (get-internal-real-time))
(format t "~&descending integers")
(cond ((<= n LIM) (format t "~&~S" sorted-integers)))
(format t "~&~S" (sorted-p sorted-integers #'>=))
(format t "~&elapsed time = ~f" (elapsed-time t1 t2))
;;; ascending reals
(format t "~&reals")
(setf reals (random-list n #'random-real))
(cond ((<= n LIM) (format t "~&~S" reals)))
(format t "~&~S" (sorted-p reals #'<=))
(setf t1 (get-internal-real-time))
(setf sorted-reals (merge-sort reals #'<=))
(setf t2 (get-internal-real-time))
(format t "~&ascending reals")
(cond ((<= n LIM) (format t "~&~S" sorted-reals)))
(format t "~&~S" (sorted-p sorted-reals #'<=))
(format t "~&elapsed time = ~f" (elapsed-time t1 t2))
;;; descending reals
(setf reals (random-list n #'random-real))
(cond ((<= n LIM) (format t "~&~S" reals)))
(format t "~&~S" (sorted-p reals #'>=))
(setf t1 (get-internal-real-time))
(setf sorted-reals (merge-sort reals #'>=))
(setf t2 (get-internal-real-time))
(format t "~&descending reals")
(cond ((<= n LIM) (format t "~&~S" sorted-reals)))
(format t "~&~S" (sorted-p sorted-reals #'>=))
(format t "~&elapsed time = ~f" (elapsed-time t1 t2))
;;; ascending strings
(format t "~&strings")
(setf records (random-records n))
(cond ((<= n LIM) (format t "~&~S" records)))
(format t "~&~S" (sorted-p records #'string<=))
(setf t1 (get-internal-real-time))
(setf sorted-records (merge-sort records #'string<=))
(setf t2 (get-internal-real-time))
(format t "~&ascending strings")
(cond ((<= n LIM) (format t "~&~S" sorted-records)))
(format t "~&~S" (sorted-p sorted-records #'string<=))
(format t "~&elapsed time = ~f" (elapsed-time t1 t2))
;;; descending strings
(setf records (random-records n))
(cond ((<= n LIM) (format t "~&~S" records)))
(format t "~&~S" (sorted-p records #'string>=))
(setf t1 (get-internal-real-time))
(setf sorted-records (merge-sort records #'string>=))
(setf t2 (get-internal-real-time))
(format t "~&descending strings")
(cond ((<= n LIM) (format t "~&~S" sorted-records)))
(format t "~&~S" (sorted-p sorted-records #'string>=))
(format t "~&elapsed time = ~f" (elapsed-time t1 t2))