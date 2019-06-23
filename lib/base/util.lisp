(in-package :lem-base)

(export '(pdebug
          utf8-bytes
          bests-if
          max-if
          min-if
          random-range
          maybe-quickload))

(defun pdebug (x &optional (file "DEBUG"))
  (with-open-file (out file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (print x out)))

(defun utf8-bytes (c)
  (cond
    ((<= c #x7f) 1)
    ((<= #xc2 c #xdf) 2)
    ((<= #xe0 c #xef) 3)
    ((<= #xf0 c #xf4) 4)
    (t 1)))

(defun bests-if (fn list test)
  (let ((best-value)
        (bests))
    (dolist (x list)
      (let ((score (funcall fn x)))
        (cond ((or (not best-value)
                   (funcall test score best-value))
               (setq best-value score)
               (setq bests (list x)))
              ((= best-value score)
               (push x bests)))))
    (values bests best-value)))

(defun max-if (fn list)
  (bests-if fn list #'>))

(defun min-if (fn list)
  (bests-if fn list #'<))

(defun random-range (min max &optional (state *random-state*))
  (+ min (random (- max min) state)))

(defun find-tree (x tree)
  (cond ((null tree) nil)
        ((eql x tree) x)
        ((consp tree)
         (or (find-tree x (car tree))
             (find-tree x (cdr tree))))))

(defun maybe-quickload (systems &rest keys &key error-on-failure-p &allow-other-keys)
  (cond
    ((uiop:featurep :quicklisp)
     (apply #'uiop:symbol-call :quicklisp :quickload systems keys))
    (t (if error-on-failure-p
           (apply #'asdf:load-systems systems)
           (ignore-errors (apply #'asdf:load-systems systems))))))
