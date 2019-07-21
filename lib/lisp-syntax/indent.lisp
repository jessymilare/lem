(defpackage :lem-lisp-syntax.indent
  (:use :cl :lem-base)
  (:export :*get-method-function*
           :get-indentation
           :set-indentation
           :update-system-indentation
           :indentation-update
           :calc-indent
           :calc-indent-region
           :&lambda))
(in-package :lem-lisp-syntax.indent)

(defparameter *body-indent* 2)
(defparameter *max-depth* 4)

(defvar *get-method-function* nil)
(defvar *static-indent-table* (make-hash-table :test 'equal))
(defvar *dynamic-indent-table* (make-hash-table :test 'equal))

(defvar *lambda-list-indentation* t)
(defvar *lambda-list-keyword-parameter-alignment* t)
(defvar *lambda-list-keyword-alignment* t)

(defvar *loop-indent-subclauses* nil)
(defvar *simple-loop-indentation* 2)

(defvar *indent-log* '())

(defvar *lisp-indent-quoted-override* t)
(defvar *lisp-indent-vector-override* t)

(defvar *region-scan-lines-limit* 100)

(declaim (inline point-next-list point-previous-list point-go-to-line
                 point-min point-max indentation-at-point))

(defun point-next-list (point &optional end-point)
  (when (scan-lists point 1 -1 t end-point)
    (character-offset point -1)
    point))

(defun point-previous-list (point &optional start-point)
  (scan-lists point -1 0 t start-point))

(defun point-go-to-list-end (point &optional end-point)
  (scan-lists point 1 1 t end-point))

(defun point-go-to-list-start (point &optional start-point)
  (scan-lists point -1 1 t start-point))

(defun point-go-to-line (point line-number &optional (charpos 0))
  (line-offset point (- line-number (line-number-at-point point)) charpos))

(defun point-min (point1 point2)
  (if (point<= point1 point2) point1 point2))

(defun point-max (point1 point2)
  (if (point>= point1 point2) point1 point2))

(defun indentation-at-point (point)
  (let ((column (point-column point)))
    (loop :for i :upfrom (- column)
          :for c := (character-at point i)
          :while (and (not (eql #\Newline c))
                      (syntax-space-char-p c))
          :finally (return (+ column i)))))

(defun get-indentation (name)
  (or (gethash name *static-indent-table*)
      (caar (gethash name *dynamic-indent-table*))))

(defun indent-method-malformed-error (name method &rest msg-args)
  (apply #'editor-error "Invalid method ~a for ~a: ~@?" method name msg-args))

(defun indent-method-welformed-p (name method &key (if-malformed :error))
  (flet ((malformed (&rest args)
           (cond
             ((eq if-malformed :error)
              (apply #'indent-method-malformed-error name method args))
             ((not if-malformed)
              (return-from indent-method-welformed-p (values nil args)))
             (t (error "unrecognised ':if-malformed' option: ~a" if-malformed)))))
    (typecase method
      ((or integer null) t)
      (null t)
      (cons
       (unless (alexandria:proper-list-p method)
         (malformed "not a proper list: ~a" method))
       (loop for (method1 . method-rest) :on method
             :finally (return t)
             :do
                (typecase method1
                  ((or integer null) nil) ; ok
                  (cons
                   (unless (eq '&whole (first method1))
                     (malformed "submethod must begin with &whole: ~a" method1))
                   (unless (and (consp (cdr method1))
                                (consp (cddr method1)))
                     (malformed "&whole must be followed by two or more elements: ~a"
                                method1))
                   (multiple-value-bind (winp error-msg-args)
                       (indent-method-welformed-p name (cdr method1) :if-malformed nil)
                     (unless winp
                       (apply #'malformed error-msg-args))))
                  (symbol
                   (case method1
                     ((&body)
                      (unless (null method-rest)
                        (malformed "&body is not the last element")))
                     ((&rest)
                      (unless (and (consp method-rest) (null (cdr method-rest)))
                        (malformed "&rest must be followed by only one element"))
                      (when (eq '&rest (car method-rest))
                        (malformed "&rest cannot be followed by another &rest")))
                     ((&whole)
                      (malformed "&whole can only be the first element in a submethod"))
                     ((&lambda) nil)
                     (t
                      (unless (fboundp method1)
                        (warn "undefined function used in method: ~a" method))))))))
      (symbol (case method
                ((&body &lambda &rest &whole)
                 (values nil '("~a should be used inside a list" method)))
                (t
                 (unless (fboundp method)
                   (warn "undefined function used as method: ~a" method))
                 t))))))

(defun set-indentation (name method)
  (when (indent-method-welformed-p name method)
    (setf (gethash name *static-indent-table*) method)))

(defun update-system-indentation (name indent packages)
  (push (list :update-system-indentation name indent packages) *indent-log*)
  (let ((list (gethash name *dynamic-indent-table*))
        ok)
    (if (null list)
        (setf (gethash name *dynamic-indent-table*)
              (list (cons indent packages)))
        (dolist (spec list)
          (cond ((equal (car spec) indent)
                 (setf (cdr spec)
                       (nunion (cdr spec) packages))
                 (setf ok t))
                (t
                 (setf (cdr spec)
                       (nset-difference (cdr spec) packages :test #'equal))))))
    (unless ok
      (setf (gethash name *dynamic-indent-table*)
            (cons (cons indent packages) list)))))

(defun indentation-update ()
  (push (list :indentation-update) *indent-log*)
  (do-all-symbols (symbol)
    (let ((key (string-downcase symbol)))
      (alexandria:when-let ((indent (swank::symbol-indentation symbol)))
        (update-system-indentation key
                                   indent
                                   (list (package-name (symbol-package symbol))))))))

(mapc (lambda (elt)
        (let ((name (car elt))
              (method (if (stringp (cdr elt))
                          (get-indentation (cdr elt))
                          (cadr elt))))
          (set-indentation name method)))
      '(("block" 1)
        ("case"        (4 &rest (&whole 2 &rest 1)))
        ("ccase" . "case")
        ("ecase" . "case")
        ("typecase" . "case")
        ("etypecase" . "case")
        ("ctypecase" . "case")
        ("catch" 1)
        ("cond"        (&rest (&whole 2 &rest 1)))
        ("defvar"      (4 2 2))
        ("defclass"    (6 (&whole 4 &rest 1) (&whole 2 &rest 1) (&whole 2 &rest 1)))
        ("defconstant" . "defvar")
        ("defcustom"   (4 2 2 2))
        ("defparameter" . "defvar")
        ("defconst"     . "defcustom")
        ("define-condition"  . "defclass")
        ("define-modify-macro" (4 &lambda &body))
        ("defsetf"     (4 &lambda 4 &body))
        ("defun"       (4 &lambda &body))
        ("defgeneric"  (4 &lambda &body))
        ("define-setf-method" . "defun")
        ("define-setf-expander" . "defun")
        ("defmacro" . "defun")
        ("defsubst" . "defun")
        ("deftype" . "defun")
        ("defmethod" lisp-indent-defmethod)
        ("defpackage"  (4 &rest (&whole 2 &rest defpackage-body)))
        ("uiop:define-package" . "defpackage")
        ("defstruct"   ((&whole 4 &rest (&whole 2 &rest 1))
                        &rest (&whole 2 &rest 1)))
        ("destructuring-bind"
         ((&whole 6 &rest 1) 4 &body))
        ;("do"          lisp-indent-do)
        ("do" 2)
        ("do*" . "do")
        ("dolist"      ((&whole 4 2 1) &body))
        ("dotimes" . "dolist")
        ("eval-when"   1)
        ("flet"        ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
        ("ignore-errors" (&rest &body))
        ("labels" . "flet")
        ("macrolet" . "flet")
        ("generic-flet" . "flet")
        ("generic-labels" . "flet")
        ("handler-case" (4 &rest (&whole 2 &lambda &body)))
        ("restart-case" . "handler-case")
        ;; `else-body' style
        ("if"          (nil nil &body))
        ;; single-else style (then and else equally indented)
        ("if"          (&rest nil))
        ;("lambda"      (&lambda &rest lisp-indent-function-lambda-hack))
        ("lambda" (&lambda &body))
        ("let"         ((&whole 4 &rest (&whole 1 2)) &body))
        ("let*" . "let")
        ("compiler-let" . "let") ;barf
        ("handler-bind" . "let")
        ("restart-bind" . "let")
        ("locally" 1)
        ("loop"         lisp-indent-loop)
        ;("loop" (&rest &body))
        (":method" lisp-indent-defmethod) ; in `defgeneric'
        (":default-initargs" (&rest 1))
        ("multiple-value-bind" ((&whole 6 &rest 1) 4 &body))
        ("multiple-value-call" (4 &body))
        ("multiple-value-prog1" 1)
        ("multiple-value-setq" (4 2))
        ("multiple-value-setf" . "multiple-value-setq")
        ("pprint-logical-block" (4 2))
        ("print-unreadable-object" ((&whole 4 1 &rest 1) &body))
        ;; Combines the worst features of BLOCK, LET and TAGBODY
        ("prog"        (&lambda &rest lisp-indent-tagbody))
        ("prog*" . "prog")
        ("prog1" 1)
        ("prog2" 2)
        ("progn" (&rest &body))
        ("progv"       (4 4 &body))
        ("return" 0)
        ("return-from" (nil &body))
        ("symbol-macrolet" . "let")
        ("tagbody"     lisp-indent-tagbody)
        ("throw" 1)
        ("unless" 1)
        ("unwind-protect" (5 &body))
        ("when" 1)
        ("with-accessors" . "multiple-value-bind")
        ("with-condition-restarts" . "multiple-value-bind")
        ("with-compilation-unit" ((&whole 4 &rest 1) &body))
        ("with-output-to-string" (4 2))
        ("with-slots" . "multiple-value-bind")
        ("with-standard-io-syntax" (2))))

(defun defpackage-body (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun loop-type (point)
  (let ((comment-split nil))
    (labels ((guard (x)
               (unless x
                 (return-from loop-type
                   (if comment-split
                       'simple/split
                       'simple)))))
      (with-point ((p point))
        (let ((line-number (line-number-at-point p))
              (maybe-split t))
          (guard (character-offset p 1))
          (guard (form-offset p 1))
          (with-point ((p p))
            (skip-whitespace-forward p)
            (if (= line-number (line-number-at-point p))
                (setf maybe-split nil)
                (setf comment-split t)))
          (guard (form-offset p 1))
          (guard (form-offset p -1))
          (if (eql (character-at p) #\()
              (if (or (not maybe-split)
                      (= line-number (line-number-at-point p)))
                  'simple
                  'simple/split)
              (if (or (not maybe-split)
                      (= line-number (line-number-at-point p)))
                  'extended
                  'extended/split)))))))

(defun trailing-comment (p)
  (and (form-offset p -1)
       (form-offset p 1)
       (progn
         (skip-whitespace-forward p t)
         (and (eql (character-at p) #\;)
              (point-column p)))))

(defun loop-macro-1 (p)
  (declare (ignore p))
  (error "unsupported ~A" '*loop-indent-subclauses*))

(defun loop-macro-keyword-p (string)
  (ppcre:scan "^(?:#?:)?(?:do|doing|finally|initially)" string))

(defun loop-part-indentation (p indent-point type)
  (labels ((f (p)
             (or (end-line-p p)
                 (eql #\) (character-at p))
                 (looking-at p "(?:#?:)?\\w+"))))
    (let ((loop-indentation (if (eq type 'extended/split)
                                (- (point-column p) 4)
                                (point-column p)))
          (indent nil))
      (back-to-indentation (move-point p indent-point))
      (cond ((eq type 'simple/split)
             (+ loop-indentation *simple-loop-indentation*))
            ((eq type 'simple)
             (+ loop-indentation 6))
            ((and (not (f p))
                  (with-point ((p p))
                    (loop :while (and (form-offset p -1)
                                      (not (f p)))
                          :do (setf indent (point-column p)))
                    (and indent (loop-macro-keyword-p (symbol-string-at-point p)))))
             indent)
            ((or #+(or) lisp-loop-indent-forms-like-keywords
                 (f p)
                 (eql #\; (character-at p)))
             (if (and (eql #\; (character-at p))
                      (alexandria:when-let ((col (trailing-comment p)))
                        (setf loop-indentation col)))
                 loop-indentation
                 (+ loop-indentation 6)))
            (t
             (+ loop-indentation 9))))))

(defun lisp-indent-loop (path indent-point sexp-column)
  (declare (ignore sexp-column))
  (if (cdr path)
      'default-indent
      (with-point ((p indent-point))
        (scan-lists p -1 1)
        (let ((type (loop-type p)))
          (if (and *loop-indent-subclauses*
                   (member type '(extended extended/split)))
              (loop-macro-1 p)
              (loop-part-indentation p
                                     (copy-point indent-point :temporary)
                                     type))))))

(defun beginning-of-defmethod-qualifiers (p)
  (let ((str nil))
    (loop
      (unless (scan-lists p -1 1) (return))
      (character-offset p 1)
      (unless (setf str (symbol-string-at-point p)) (return))
      (cond ((string-equal str "defmethod")
             (form-offset p 2)
             (return 1))
            ((string-equal str ":method")
             (form-offset p 1)
             (return 0)))
      (character-offset p -1))))

(defun lisp-indent-defmethod (path indent-point sexp-column)
  (with-point ((p indent-point))
    (compute-indent-method
     (let ((nskip (beginning-of-defmethod-qualifiers p)))
       (cond (nskip
              (skip-whitespace-forward p)
              (loop :while (syntax-symbol-char-p (character-at p))
                    :do (incf nskip)
                        (form-offset p 1)
                        (skip-whitespace-forward p))
              (append (make-list nskip :initial-element 4) '(&lambda &body)))
             (t
              (get-indentation "defun"))))
     path indent-point sexp-column)))

(defun lisp-indent-do (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun lisp-indent-function-lambda-hack (path indent-point sexp-column)
  (declare (ignore path indent-point sexp-column))
  'default-indent)

(defun lisp-indent-tagbody (path indent-point sexp-column)
  (declare (ignore path))
  (with-point ((indent-point indent-point))
    (if (symbol-string-at-point (back-to-indentation indent-point))
        (+ sexp-column 1)
        (+ sexp-column *body-indent*))))

(defun lisp-indent-keyword (path indent-point sexp-column)
  (declare (ignore path indent-point))
  (+ sexp-column 1))

(defun lisp-indent-quoted-form (path indent-point sexp-column)
  (declare (ignore path indent-point))
  (+ sexp-column 1))

(defun lisp-indent-sexp-with-key (path indent-point sexp-column)
  (declare (ignore path indent-point))
  (+ sexp-column 1))

(defun lisp-indent-sexp-with-string (path indent-point sexp-column)
  (declare (ignore path indent-point))
  (+ sexp-column 1))

(defun lisp-indent-sexp-with-conditional-read (path indent-point sexp-column)
  (declare (ignore path indent-point))
  (+ sexp-column 1))

(defun lisp-indent-vector-form (path indent-point sexp-column)
  (declare (ignore path indent-point))
  (+ sexp-column 1))

(defun lambda-list-keyword-p (name)
  (and (stringp name)
       (find name
             '("&optional" "&rest" "&key" "&allow-other-keys" "&aux"
               "&whole" "&body" "&environment")
             :test #'string-equal)))

(defun search-lambda-list-keyword (p)
  (loop
    (unless (form-offset p -1)
      (return nil))
    (when (lambda-list-keyword-p (symbol-string-at-point p))
      (return p))))

(defun compute-indent-lambda-list (path indent-point sexp-column)
  (declare (ignore path))
  (unless *lambda-list-indentation*
    (return-from compute-indent-lambda-list (1+ sexp-column)))
  (with-point ((p indent-point))
    (cond
      ((progn
         (back-to-indentation p)
         (lambda-list-keyword-p (symbol-string-at-point p)))
       (if *lambda-list-keyword-alignment*
           (if (search-lambda-list-keyword p)
               (point-column p)
               (1+ sexp-column))
           (1+ sexp-column)))
      (t
       (cond
         ((search-lambda-list-keyword p)
          (if *lambda-list-keyword-parameter-alignment*
              (if (looking-at p "[\\w&]+\\s*$")
                  (point-column p)
                  (+ 1 (point-column (form-offset p 1))))
              (+ 2 (point-column p))))
         (t
          (1+ sexp-column)))))))

(defun compute-indent-integer-method (method path indent-point sexp-column)
  (declare (ignore indent-point))
  (cond ((cdr path)
         'default-indent)
        ((<= (car path) method)
         (+ sexp-column 4))
        (t
         (+ sexp-column *body-indent*))))

(defun compute-indent-symbol-method (method path indent-point sexp-column)
  (funcall method path indent-point sexp-column))

(defun compute-indent-complex-method (method path indent-point sexp-column)
  (if (consp (cdr path))
      (let ((submethod (compute-indent-complex-submethod method (car path))))
        (typecase submethod
          ((member nil default-indent) 'default-indent)
          (symbol (compute-indent-symbol-method submethod path
                                                indent-point sexp-column))
          (cons (compute-indent-complex-method submethod (cdr path)
                                               indent-point sexp-column))))
      (let ((n-start (car path)))
        (unless (and (= n-start 0) (eq '&rest (car method)))
          (decf n-start))
        (loop :with restp := nil
              :for (method1 . method-rest) :on method
              :for n :from n-start :downto 0
              :if (eq method1 '&rest) :do
                 (setq method1 (car method-rest)
                       method-rest nil
                       restp (> n 0)
                       n 0)
              :if (eq method1 '&body) :do
                 (return ;; Indent sexps in &body
                   (+ sexp-column *body-indent*))
              :if (zerop n) :do
                 (return
                   (cond
                     ((integerp method1) (+ method1 sexp-column))
                     ((eq method1 '&lambda) (+ 4 sexp-column))
                     ((not method1) 'default-indent)
                     ((symbolp method1) (compute-indent-symbol-method method1 path
                                                                      indent-point
                                                                      sexp-column))
                     ((consp method1)
                      (if restp
                          'default-indent
                          (let ((method1 (cadr method1)))
                            (if (integerp method1)
                                (+ method1 sexp-column)
                                (compute-indent-symbol-method method1 path
                                                              indent-point
                                                              sexp-column)))))))))))

(defun compute-indent-complex-submethod (method n-start)
  ;; When n-or-nil is 'nil', return indent of current sexp
  (unless (and (= n-start 0) (eq '&rest (car method)))
    (decf n-start))
  (loop :with restp := nil
        :for (method1 . method-rest) :on method
        :for n :from n-start :downto 0
        :if (eq method1 '&rest) :do
           (setq method1 (car method-rest)
                 method-rest nil
                 restp (> n 0)
                 n 0)
        :if (eq method1 '&body) :do
           (return ;; Indent subsexps in &body
             'default-indent)
        :if (zerop n) :do
           (return (cond
                     ((consp method1) (cddr method1))
                     ((integerp method1) 'default-indent)
                     ((eq method1 '&lambda) 'compute-indent-lambda-list)
                     ((not method1) 'default-indent)
                     ((symbolp method1) method1)))))

(defun compute-indent-method (method path indent-point sexp-column
                              &optional (default 'default-indent))
  (if (or (not method) (eq method 'default-indent))
      default
      (let ((indent (funcall (etypecase method
                               (integer #'compute-indent-integer-method)
                               (symbol #'compute-indent-symbol-method)
                               (list #'compute-indent-complex-method))
                             method path indent-point sexp-column)))
        (if (or (not indent) (eq indent 'default-indent))
            default
            indent))))

(defun quote-form-point-p (p)
  (and (eql (character-at p -1) #\')
       (not (eql (character-at p -2) #\#))))

(defun vector-form-point-p (p)
  (eql (character-at p -1) #\#))

(defun find-indent-method (name &optional path)
  (declare (ignore path))
  (when name
    (let ((name (string-downcase name)))
      (flet ((f (method)
               (when method
                 (return-from find-indent-method method))))
        (f (get-indentation name))
        (let ((name1 (ppcre:scan-to-strings "(?<=:)[^:]+" name)))
          (when name1
            (f (get-indentation name1)))
          (f (and *get-method-function*
                  (funcall *get-method-function* name)))
          (f (and (ppcre:scan "^(?:with-|without-|within-|do-|def)" (or name1 name))
                  '(4 &body))))))))

(defun find-sexp-indent-method (sexp-point &optional parent-method)
  (cond
    ((quote-form-point-p sexp-point)
     'lisp-indent-quoted-form)
    ((vector-form-point-p sexp-point)
     'lisp-indent-vector-form)
    ((or (and (eq parent-method 'lisp-indent-quoted-form)
              *lisp-indent-quoted-override*)
         (and (eq parent-method 'lisp-indent-vector-form)
              *lisp-indent-vector-override*))
     nil)
    (t
     (with-point ((point sexp-point))
       ;; Go to first form in sexp
       (character-offset point 1)
       (skip-space-and-comment-forward point)
       (let ((name (symbol-string-at-point point)))
          (or (find-indent-method name '(1))
              (cond
                ((eql (character-at point) #\:)
                 (when (find parent-method '(nil default-indent))
                   'lisp-indent-sexp-with-key))
                ((eql (character-at point) #\")
                 'lisp-indent-sexp-with-string)
                ((looking-at point "#!?[+-]")
                 'lisp-indent-sexp-with-conditional-read)
                (t nil))))))))

;; (defun calc-function-indent (point)
;;   (loop
;;     (unless (form-offset point -1)
;;       (let ((charpos (point-charpos point)))
;;         (form-offset point 1)
;;         (skip-whitespace-forward point t)
;;         (when (or (eql #\; (character-at point))
;;                   (end-line-p point))
;;           (line-offset point 0 charpos)))
;;       (return))
;;     (let ((charpos (point-charpos point)))
;;       (back-to-indentation point)
;;       (when (= charpos (point-charpos point))
;;         (return))
;;       (line-offset point 0 charpos)))
;;   (point-column point))

(defun calc-indent-region (start-point end-point)
  (if (point< end-point start-point)
      (rotatef start-point end-point))
  (line-start start-point)
  (line-start end-point)
  (with-point ((sexp-point start-point :temporary)
               (sexp-point-at-end start-point :temporary))
    (line-offset sexp-point -1)
    ;; Try finding a toplevel form before start-point,
    ;; which is a lot faster than scan-lists
    (loop :while (and (not (syntax-open-paren-char-p (character-at sexp-point)))
                      (line-offset sexp-point -1))
          :repeat *region-scan-lines-limit*)
    (cond
      ;; Found toplevel form?
      ((syntax-open-paren-char-p (character-at sexp-point)))
      ;; Is start-point at toplevel?
      ((syntax-open-paren-char-p (character-at start-point))
       (move-point sexp-point start-point))
      ;; If no toplevel form was found, scan to a parent sexp
      (t
       (move-point sexp-point start-point)
       (with-point ((limit sexp-point :temporary))
         (let ((limit-arg (line-offset limit (- *region-scan-lines-limit*))))
           (point-go-to-list-start sexp-point limit-arg)))))
    ;; Dispatch accordingly
    (let* ((line-start-num (line-number-at-point start-point))
           (line-end-num   (1+ (line-number-at-point end-point)))
           (indent-vector  (make-array (- line-end-num line-start-num)
                                       :initial-element 0))
           (sexp-column 0))
      (when (and (point= start-point sexp-point)
                 (in-string-or-comment-p sexp-point))
        (setf (svref indent-vector 0)
              (setf sexp-column (indentation-at-point sexp-point))))
      (with-point ((indent-point start-point :temporary))
        (loop :while (and (move-point sexp-point-at-end sexp-point)
                          (form-offset sexp-point-at-end 1))
              :do (when (point<= indent-point sexp-point-at-end)
                    (%calc-indent/vector sexp-point sexp-column
                                         indent-point end-point
                                         indent-vector line-start-num
                                         'default-indent nil))
              :while (point<= indent-point end-point)
              :do (move-point sexp-point sexp-point-at-end)
                  (unless (point-next-list sexp-point)
                    (line-offset sexp-point 1)
                    (if (point<= sexp-point end-point)
                        (setf (svref indent-vector (- (line-number-at-point sexp-point)
                                                      line-start-num))
                              (indentation-at-point sexp-point))
                        (return)))
                  (setf sexp-column
                        (%compute-indent/vector 'default-indent nil
                                                indent-point 0 sexp-column
                                                (point-min sexp-point end-point)
                                                indent-vector line-start-num))
              :while (point<= indent-point end-point)))
      indent-vector)))

(defun region-indent-line-p (point)
  (not (in-string-or-comment-p point)))

(defun %compute-indent/vector (method path indent-point sexp-column default-column
                               end-point indent-vector line-start-num)
  ;; (log:info "compute" method path sexp-column default-column indent-point)
  (let ((indent-line (line-number-at-point indent-point)))
    (if (<= line-start-num indent-line)
        (loop :while (point<= indent-point end-point)
              :for i := (- indent-line line-start-num)
              :do (if (region-indent-line-p indent-point)
                      (setf default-column
                            (compute-indent-method method path indent-point
                                                   sexp-column default-column)
                            (svref indent-vector i) default-column)
                      (setf (svref indent-vector i) (indentation-at-point indent-point)))
              :while (and (line-offset indent-point 1)
                          (< indent-line (line-number-at-point indent-point)))
              :do (incf indent-line)
              :finally (return default-column))
        default-column)))

(defun %point-column-after-indent (point indent-vector line-start-num end-point)
  (let* ((point-line   (line-number-at-point point))
         (point-column (point-column point)))
    (if (and (<= line-start-num point-line)
             (point<= point end-point))
        (+ point-column ; remove old indent and insert new one
           (- (indentation-at-point point))
           (svref indent-vector (- point-line line-start-num)))
        point-column)))

(defun %calc-indent/vector (sexp-point sexp-column indent-point end-point
                            indent-vector line-start-num
                            parent-method parent-rev-path)
  (with-point ((point sexp-point :temporary)
               (point-at-end sexp-point :temporary))
    ;; Go inside sexp
    (character-offset point 1)
    (character-offset point-at-end 1)
    (skip-space-and-comment-forward point)

    (let* ((method (find-sexp-indent-method sexp-point parent-method))
           (parent-rev-path (if method '() parent-rev-path))
           (method (or method parent-method)))
      (loop :with default-column := (1+ sexp-column)
            :with last-loop := nil
            :for previous-line := (line-number-at-point sexp-point) :then point-line
            :for point-line := (line-number-at-point point)
            :for n :from 0
            :for rev-path := (cons n parent-rev-path)
            :do
               (unless (form-offset point-at-end 1)
                 (setq last-loop t))
               (cond
                 ((< previous-line point-line)
                  ;; New line - need indenting
                  ;; Indent at most to end-point
                  (if (<= line-start-num point-line)
                      (let ((path (reverse rev-path)))
                        (setq default-column
                              (%compute-indent/vector method path indent-point
                                                      sexp-column default-column
                                                      (point-min point end-point)
                                                      indent-vector line-start-num))
                        (unless (point< point indent-point)
                          (line-offset indent-point 1)))
                      ;; Don't indent yet
                      (setq default-column (point-column point))))
                 ((= n 0)
                  ;; Don't need to indent, but need to compute 'default-column'
                  (with-point ((aux point :temporary))
                    (let ((ref-point (if (and (form-offset aux 1)
                                              (skip-whitespace-forward aux t)
                                              (not (end-line-p aux))
                                              (= point-line (line-number-at-point aux)))
                                         aux
                                         point)))
                      (setq default-column (%point-column-after-indent
                                            ref-point indent-vector line-start-num
                                            end-point))))))
               (skip-chars-forward point #'syntax-expr-prefix-char-p)
               ;; Sexp?
            :while (point<= indent-point end-point)
            :do (when (point<= indent-point point-at-end)
                  (skip-chars-forward point #'syntax-expr-prefix-char-p)
                  (when (syntax-open-paren-char-p (character-at point))
                    (let ((rev-path (if (or (consp method) (integerp method))
                                        '()
                                        rev-path))
                          (method (cond
                                    ((consp method)
                                     (compute-indent-complex-submethod method n))
                                    ((integerp method) 'default-indent)
                                    (t  method)))
                          (sexp-column (%point-column-after-indent
                                        point indent-vector line-start-num
                                        end-point)))
                      (%calc-indent/vector point sexp-column indent-point end-point
                                           indent-vector line-start-num
                                           method rev-path))))
            :while (and (point<= indent-point end-point)
                        (not last-loop))
            :do (move-point point point-at-end)
                (skip-space-and-comment-forward point)
                (move-point point-at-end point)))))

;; (defun calc-indent-1 (indent-point)
;;   (let* ((const-flag nil)
;;          (innermost-sexp-column nil)
;;          (calculated
;;            (with-point ((p indent-point))
;;              (loop
;;                :named outer
;;                :with path := '() :and sexp-column
;;                :for innermost := t :then nil
;;                :repeat *max-depth*
;;                :do
;;                   (loop :for n :from 0 :do
;;                            (when (and (< 0 n) (start-line-p p))
;;                              (return-from outer nil))
;;                            (unless (form-offset p -1)
;;                              (push n path)
;;                              (return)))
;;                   (when (and (null (cdr path))
;;                              (= 0 (car path))
;;                              (scan-lists p -1 1 t))
;;                     (return-from outer (1+ (point-column p))))
;;                   (when (and innermost
;;                              (or (member (character-at p 0) '(#\: #\"))
;;                                  (looking-at p "#!?[+-]")))
;;                     (setf const-flag t))
;;                   (let ((name (symbol-string-at-point p)))
;;                     (unless (scan-lists p -1 1 t)
;;                       (return-from outer 'default-indent))
;;                     (unless sexp-column (setf sexp-column (point-column p)))
;;                     (when (or (quote-form-point-p p)
;;                               (vector-form-point-p p))
;;                       (return-from outer (1+ sexp-column)))
;;                     (when innermost
;;                       (setf innermost-sexp-column sexp-column))
;;                     (let ((method (find-indent-method name path)))
;;                       (when method
;;                         (return-from outer (compute-indent-method method
;;                                                                   path
;;                                                                   indent-point
;;                                                                   sexp-column)))))))))
;;     (if (or (null calculated)
;;             (eq calculated 'default-indent))
;;         (if (and const-flag innermost-sexp-column)
;;             (1+ innermost-sexp-column)
;;             (calc-function-indent indent-point))
;;         calculated)))

(defun calc-indent (point)
  (line-start point)
  (lem-base::with-point-syntax point
    (let ((state (syntax-ppss point)))
      (cond
        ((pps-state-string-p state) nil)
        ((zerop (pps-state-paren-depth state))
         0)
        (t (aref (calc-indent-region point point) 0))))))
