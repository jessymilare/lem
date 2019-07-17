(in-package :lem-base)

(export '(kill-buffer-hook
          buffer-list
          any-modified-buffer-p
          get-buffer
          new-buffer-name
          new-buffer-name-function
          new-buffer-name/number
          unbury-buffer
          bury-buffer
          get-next-buffer
          get-previous-buffer
          delete-buffer
          get-file-buffer))

(define-editor-variable kill-buffer-hook '())

(defvar *buffer-list* '())

(defun buffer-list ()
  "`buffer`のリストを返します。"
  *buffer-list*)

(defun set-buffer-list (buffer-list)
  (setf *buffer-list* buffer-list))

(defun add-buffer (buffer)
  (check-type buffer buffer)
  (assert (not (get-buffer (buffer-name buffer))))
  (set-buffer-list (cons buffer (buffer-list))))

(defun any-modified-buffer-p ()
  (find-if (lambda (buffer)
             (and (buffer-filename buffer)
                  (buffer-modified-p buffer)))
           (buffer-list)))

(defun get-buffer (buffer-or-name)
  "`buffer-or-name`がバッファならそのまま返し、
文字列ならその名前のバッファを返します。"
  (if (bufferp buffer-or-name)
      buffer-or-name
      (find-if #'(lambda (buffer)
                   (string= buffer-or-name
                            (buffer-name buffer)))
               (buffer-list))))

(define-editor-variable new-buffer-name-function
  'new-buffer-name/number
  "Defines a function that is called before creating a buffer with a unique name.
 It is called several times until it finds a name that is not used by any other
 buffer.
 This function must receive three arguments:
   `name`, which is the proposed name for the new buffer;
   `namestring`, representing the pathname of the file being opened in buffer or
     `nil` if buffer will not contain a filename.
   `trial-number`, a non-negative integer, initially zero, that increases at each
     call for creating one buffer. It can be used to compose the buffer name.
 It is safe to simply return `name` when `trial-number`` is zero. This function may
 rename existing buffers in case of conflicts (first ensuring no buffer with that
 name exists) and must return a string for the new buffer name.")

(defun new-buffer-name (name namestring)
  (let ((create (variable-value 'new-buffer-name-function)))
    (labels ((rec (n)
               (let ((name (funcall create name namestring n)))
                 (check-type name string "Invalid buffer name")
                 (if (not (get-buffer name))
                     name
                     (rec (1+ n))))))
      (rec 0))))

(defun new-buffer-name/number (name namestring trial-number)
  (declare (ignore namestring))
  (format nil "~a~[~:;~:*<~d>~]" name trial-number))

(defun delete-buffer (buffer)
  "`buffer`をバッファのリストから消します。
エディタ変数`kill-buffer-hook`がバッファが消される前に実行されます。"
  (check-type buffer buffer)
  (alexandria:when-let ((hooks (variable-value 'kill-buffer-hook :buffer buffer)))
    (run-hooks hooks buffer))
  (alexandria:when-let ((hooks (variable-value 'kill-buffer-hook :global)))
    (run-hooks hooks buffer))
  (buffer-free buffer)
  (set-buffer-list (delete buffer (buffer-list))))

(defun get-next-buffer (buffer)
  "バッファリスト内にある`buffer`の次のバッファを返します。"
  (check-type buffer buffer)
  (let* ((buffer-list (buffer-list))
         (res (member buffer buffer-list)))
    (cadr res)))

(defun get-previous-buffer (buffer)
  "バッファリスト内にある`buffer`の前のバッファを返します。"
  (check-type buffer buffer)
  (loop :for prev := nil :then curr
        :for curr :in (buffer-list)
        :do (when (eq buffer curr)
              (return prev))))

(defun unbury-buffer (buffer)
  (check-type buffer buffer)
  (unless (buffer-temporary-p buffer)
    (set-buffer-list
     (cons buffer
           (delete buffer (buffer-list)))))
  buffer)

(defun bury-buffer (buffer)
  "`buffer`をバッファリストの一番最後に移動させ、バッファリストの先頭を返します。"
  (check-type buffer buffer)
  (unless (buffer-temporary-p buffer)
    (set-buffer-list
     (nconc (delete buffer (buffer-list))
            (list buffer))))
  (car (buffer-list)))

(defun get-file-buffer (filename)
  "`filename`に対応するバッファを返します。
見つからなければNILを返します。"
  (dolist (buffer (buffer-list))
    (when (uiop:pathname-equal filename (buffer-filename buffer))
      (return buffer))))
