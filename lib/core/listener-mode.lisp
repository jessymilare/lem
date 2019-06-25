(defpackage :lem.listener-mode
  (:use :cl :lem)
  (:export :listener-prompt-attribute
           :*listener-mode-keymap*
           :listener-start
           :listener-update-point
           :listener-reset-prompt
           :listener-return
           :listener-prev-input
           :listener-next-input
           :listener-reset-interactive
           :listener-set-prompt-function
           :listener-check-input-function
           :listener-execute-function
           :clear-listener)
  #+sbcl
  (:lock t))
(in-package :lem.listener-mode)

(define-attribute listener-prompt-attribute
  (:light :foreground "blue" :bold-p t)
  (:dark :foreground "cyan" :bold-p t))

(defvar %listener-point-indicator (gensym))
(defmacro %listener-point (buffer)
  `(buffer-value ,buffer %listener-point-indicator))

(defvar %listener-history-indicator (gensym))
(defmacro %listener-history ()
  `(buffer-value (current-buffer) %listener-history-indicator))

(define-editor-variable listener-set-prompt-function)
(define-editor-variable listener-check-input-function)
(define-editor-variable listener-execute-function)

(define-minor-mode listener-mode
    (:name "listener"
     :keymap *listener-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) nil)
  (unless (%listener-history)
    (setf (%listener-history)
          (lem.history:make-history))))

(define-key *listener-mode-keymap* "Return" 'listener-return)
(define-key *listener-mode-keymap* "M-p" 'listener-prev-input)
(define-key *listener-mode-keymap* "M-n" 'listener-next-input)
(define-key *listener-mode-keymap* "M-r" 'listener-previous-matching-input)
(define-key *listener-mode-keymap* "C-c M-o" 'listener-clear-buffer)
(define-key *listener-mode-keymap* "C-c C-u" 'listener-clear-input)

(defun listener-start-point (buffer)
  (%listener-point buffer))

(defun listener-start (buffer-name mode)
  (let ((buffer (make-buffer buffer-name)))
    (setf (current-window) (pop-to-buffer buffer))
    (funcall mode)
    (listener-reset-prompt buffer)))

(defun listener-update-point (&optional (point (current-point)))
  (when (%listener-point (point-buffer point))
    (delete-point (%listener-point (point-buffer point))))
  (setf (%listener-point (point-buffer point))
        (if point
            (copy-point point :right-inserting)
            (copy-point (current-point) :right-inserting))))

(defun listener-reset-prompt (&optional (buffer (current-buffer)) (fresh-line t))
  (let ((cur-point (buffer-point buffer)))
    (buffer-end cur-point)
    (when fresh-line
      (unless (start-line-p cur-point)
        (insert-character cur-point #\newline 1)
        (buffer-end cur-point)))
    (let ((point (funcall (variable-value 'listener-set-prompt-function
                                          :buffer buffer)
                          cur-point)))
      (with-point ((s point))
        (line-start s)
        (put-text-property s point :attribute 'listener-prompt-attribute)
        (put-text-property s point :read-only t)
        (put-text-property s point :field t)))
    (buffer-end cur-point)
    (buffer-undo-boundary buffer)
    (listener-update-point cur-point)))

(define-command listener-return () ()
  (with-point ((point (buffer-end (current-point)) :left-inserting))
    (if (not (funcall (variable-value 'listener-check-input-function) point))
        (insert-character point #\newline)
        (let ((start (listener-start-point (current-buffer))))
          (unless (point<= start point)
            (listener-reset-prompt)
            (return-from listener-return t))
          (let ((str (points-to-string start point)))
            (lem.history:add-history (%listener-history) str)
            (buffer-end point)
            (insert-character point #\newline)
            (listener-update-point)
            (funcall (variable-value 'listener-execute-function) point str)))))
  t)

(defun %backup-edit-string (history)
  (lem.history:backup-edit-string
   history
   (points-to-string (listener-start-point (current-buffer))
                     (buffer-end-point (current-buffer)))))

(defun %restore-edit-string (history)
  (multiple-value-bind (str win)
      (lem.history:restore-edit-string history)
    (when win
      (replace-textarea str))))

(defun replace-textarea (str)
  (let ((start (listener-start-point (current-buffer)))
        (end (buffer-end-point (current-buffer))))
    (save-excursion
      (delete-between-points start end)
      (insert-string start str)
      (move-point (%listener-point (current-buffer)) start))
    (buffer-end (current-point))))

(define-command listener-prev-input () ()
  (%backup-edit-string (%listener-history))
  (multiple-value-bind (str win)
      (lem.history:prev-history (%listener-history))
    (when win
      (replace-textarea str))))

(define-command listener-next-input () ()
  (%backup-edit-string (%listener-history))
  (%restore-edit-string (%listener-history))
  (multiple-value-bind (str win)
      (lem.history:next-history (%listener-history))
    (when win
      (replace-textarea str))))

(define-command listener-previous-matching-input (regexp)
    ((list (prompt-for-string "Previous element matching (regexp): ")))
  (%backup-edit-string (%listener-history))
  (multiple-value-bind (str win)
      (lem.history:previous-matching (%listener-history) regexp)
    (when win
      (replace-textarea str))))

(defun clear-listener (buffer)
  (let ((*inhibit-read-only* t))
    (erase-buffer buffer))
  (listener-reset-prompt buffer))

(define-command listener-clear-buffer () ()
  (clear-listener (current-buffer))
  t)

(define-command listener-clear-input () ()
  (delete-between-points (listener-start-point (current-buffer))
                         (buffer-end-point (current-buffer))))
