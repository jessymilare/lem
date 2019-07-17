(in-package :lem)

(export '(*auto-mode-alist*
          *\#!-alist*
          new-buffer-name-function
          new-buffer-name/number
          new-buffer-name/directory
          *trusted-new-buffer-name-functions*
          *buffer-name/directory-max-dir-length*))

(defvar *auto-mode-alist* nil)
(defvar *\#!-alist*
  '(("env" . second)))

(defun scan-var/val (str start)
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan "\\s*([a-zA-Z0-9-_]+)\\s*:\\s*" str :start start)
    (when start
      (let ((var (subseq str
                         (aref reg-starts 0)
                         (aref reg-ends 0))))
        (multiple-value-bind (val end)
            (handler-bind ((error (lambda (c)
                                    (declare (ignore c))
                                    (return-from scan-var/val))))
              (let ((*read-eval* nil))
                (read-from-string str nil nil :start end)))
          (values end var val))))))

(defun set-file-property (buffer var val)
  (cond ((string-equal var "mode")
         (let ((mode (find-mode-from-name val)))
           (when mode
             (change-buffer-mode buffer mode))))
        (t
         (let ((ev (find-editor-variable var)))
           (if ev
               (setf (variable-value ev :buffer buffer) val)
               (setf (buffer-value buffer (string-downcase var)) val))))))

(defun scan-line-property-list (buffer str)
  (loop :with i := 0
        :do (multiple-value-bind (pos var val)
                (scan-var/val str i)
              (unless pos (return))
              (set-file-property buffer var val)
              (setf i pos))))

(defun change-buffer-mode-\#! (buffer line)
  (let* ((* (uiop:split-string line :separator " "))
         (* (remove 0 * :key #'length))
         (cmd (first (last (uiop:split-string (first *) :separator "/"))))
         (mode #1=(cdr (assoc cmd *\#!-alist* :test #'ppcre:scan)))
         ;;re-eval for 'env' like command
         (mode (or (find mode *mode-list*)
                   (and mode
                        (setf cmd (funcall mode *))
                        #1#)))
         (mode (if mode
                   (find mode *mode-list*)
                   (find-mode-from-name cmd))))
    (when mode
      (change-buffer-mode buffer mode))))

(defun scan-file-property-list (buffer)
  (with-point ((cur-point (buffer-point buffer)))
    (buffer-start cur-point)
    (when (ppcre:scan "^#!" (line-string cur-point))
      (change-buffer-mode-\#! buffer (line-string cur-point))
      (line-offset cur-point 1))
    (loop :until (end-line-p cur-point)
          :for string := (line-string cur-point)
          :do (ppcre:register-groups-bind (result)
                  ("-\\*-(.*)-\\*-" string)
                (when result
                  (scan-line-property-list buffer result)
                  (return)))
              (if (string= "" (string-trim '(#\space #\tab) string))
                  (line-offset cur-point 1)
                  (return)))))

(defun prepare-auto-mode (buffer)
  (let* ((filename (file-namestring (buffer-filename buffer)))
         (elt (find-if (lambda (elt)
                         (ppcre:scan (car elt) filename))
                       *auto-mode-alist*)))
    (when elt
      (change-buffer-mode buffer (cdr elt)))))

(defun detect-external-format-from-file (pathname)
  (values (inq:dependent-name (inq:detect-encoding (pathname pathname) :jp))
          (or (inq:detect-end-of-line (pathname pathname)) :lf)))

(setf *external-format-function* 'detect-external-format-from-file)


;;; Resolving buffer name conflicts using directory names
(defvar *trusted-new-buffer-name-functions*
  '(new-buffer-name/number new-buffer-name/directory))
(defvar *buffer-name/directory-max-dir-length* 3)

(defun new-buffer-name/directory (name namestring trial-number)
  "This function can be set to `(variable-value 'new-buffer-name-function)` for
 using directory names when two or more files with same name are openned."
  (cond
    ((or (null namestring) (eql "" namestring))
     (new-buffer-name/number name namestring trial-number))
    ((zerop trial-number)
     name)
    (t
     (alexandria:if-let (buffers (list-buffers-by-file-name name))
       (let* ((number-of-dirs trial-number)
              (max-dir-list *buffer-name/directory-max-dir-length*)
              (dir-list1 (namestring-dir-list namestring max-dir-list)))
         ;; Let's find how many dirs we need to resolve conflicts
         (dolist (buffer2 buffers)
           (let* ((namestring2 (buffer-filename buffer2))
                  (dir-list2   (namestring-dir-list (or namestring2 "")
                                                    max-dir-list))
                  (mismatch (or (mismatch dir-list1 dir-list2 :test #'string=
                                          :from-end t) 0))
                  ;; Number of dirs already present in buffer2 name
                  (dirs-in-name2 (count-dirs-in-name (buffer-name buffer2)))
                  ;; Minimum number of dirs to resolve conflict
                  (new-n-dirs (- (length dir-list1) -1 mismatch)))
             ;; Increase `number-of-dirs` if necessary
             (unless (<= new-n-dirs number-of-dirs)
               (setf number-of-dirs new-n-dirs))
             ;; Update name of `buffer2` if it doesn't contain enough directories to
             ;; differenciate it with the new buffer
             (unless (>= dirs-in-name2 new-n-dirs)
               (let ((new-name2 (find-new-buffer-name/directory
                                 name (last dir-list2 number-of-dirs))))
                 (buffer-rename buffer2 new-name2)))))
         ;; Finally generate new name
         (find-new-buffer-name/directory name (last dir-list1 number-of-dirs)))
       ;; No conflicts were found
       (new-buffer-name/number name namestring trial-number)))))

(defun list-buffers-by-file-name (file-name)
  (remove-if-not (lambda (buffer)
                   (let ((namestring (buffer-filename buffer)))
                     (and namestring (string= file-name (file-namestring namestring)))))
                 (buffer-list)))

(defun namestring-dir-list (namestring &optional max)
  (let* ((namestring (remove-trailing-/ namestring))
         (pathname (pathname namestring))
         (dir-list (remove-if-not #'stringp (pathname-directory pathname))))
    (if (not max) dir-list (last dir-list *buffer-name/directory-max-dir-length*))))

(defun remove-trailing-/ (namestring)
  (if (or (char= #\/ (alexandria:last-elt namestring))
          (char= #\\ (alexandria:last-elt namestring)))
      (subseq namestring 0 (1- (length namestring)))
      namestring))

(defun make-buffer-name/directory (name dirs &optional number)
  (if (eql number 0) (setf number nil))
  (format nil "~A<dir: ~{~A/~}>~@[<~A>~]"
          name dirs number))

(defun count-dirs-in-name (name)
  (alexandria:if-let ((pos (position #\< name)))
    (let ((pos (cl-ppcre:scan '(:sequence "dir:") name :start pos))
          (end (cl-ppcre:scan #\> name :start pos)))
      (floor (length (cl-ppcre:all-matches '(:alternation #\\ #\/) name
                                           :start pos
                                           :end end))
             2))
    0))

(defun find-new-buffer-name/directory (name dirs &optional number)
  (let* ((new-name (make-buffer-name/directory name dirs number))
         (number (or number 0)))
    (if (get-buffer new-name)
        (find-new-buffer-name/directory name dirs (1+ number))
        new-name)))
