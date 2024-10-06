;; TODOs:
;; - Do better pretty printing of the PTX instead of just using format,
;;   ideally things would automatically get indented if there is nesting

(defun emit-printf (fmt i)
  (let* ((depot (string (gensym "local_depot"))))
    (format t "{~%")
    ;; TODO: format string is hardcoded for now, derive it from fmt instead
    (format t "  .global .align 1 .b8 $str[4] = {37, 100, 10};~%")
    (format t "  .local .align 8 .b8 ~a;~%" depot)
    (format t "  .reg .b64 %SP;~%")
    (format t "  .reg .b64 %SPL;~%")
    (format t "  .reg .u64 %str;~%")
    (format t "  .reg .b32 %r;~%")
    (format t "  .reg .b64 %rd<3>;~%")
    (format t "  mov.u64 %SPL, ~a;~%" depot)
    (format t "  cvta.local.u64 %SP, %SPL;~%")
    (format t "  add.u64 %rd1, %SP, 0;~%")
    (format t "  add.u64 %rd2, %SPL, 0;~%")
    (format t "  mov.u32 %r, ~a;~%" i)
    (format t "  st.local.u32 [%rd2], %r;~%")
    (format t "  .param .b64 param0;~%")
    (format t "  .param .b64 param1;~%")
    (format t "  .param .b32 retval;~%")
    (format t "  cvta.global.u64 %str, $str;~%")
    (format t "  st.param.b64 [param0+0], %str;~%")
    (format t "  st.param.u64 [param1+0], %rd1;~%")
    (format t "  call.uni (retval), vprintf, (param0, param1);~%")
    (format t "}~%")))

;; (emit-printf "%d" 0)

;; This is currently very specialized for printf, need to generalize
(defmacro call-func (function &rest args)
  `(progn
     (.param ".b64 param0")
     (.param ".b64 param1")
     (.param ".b32 retval")
     (%st.param.b64 "[param0+0]" ,(nth 0 args))
     (%st.param.u64 "[param1+0]" ,(nth 1 args))
     (%call.uni "(retval)" ,function "(param0, param1)")))

;; TODO: Add some more error checking here. Also, this should be made a lot more generic
;; (e.g. currently the format string is hardcoded which is easy to change)
(defmacro printf (fmt &rest args)
  (let* ((depot (string (gensym "local_depot")))
	 (s (format nil "{~{~a~^, ~}}" (map 'list #'char-code fmt))))
    `(progn
       (.global ,(format nil ".align 1 .b8 $str[~a] = ~a" (length s) s))
       (.local ,(format nil ".align 8 .b8 ~a" depot))
       (.reg ".b64 %sp")
       (.reg ".b64 %spl")
       (.reg ".b32 %r")
       (.reg ".b64 %rd<3>")
       (.reg ".u64 %str")
       (%mov.u64 %spl ,depot)
       (%cvta.local.u64 %SP %spl)
       (%add.u64 %rd1 %sp 0)
       (%add.u64 %rd2 %spl 0)
       (%mov.u32 %r ,(nth 0 args))
       (%st.local.u32 "[%rd2]" %r)
       (%cvta.global.u64 %str $str)
       (call-func vprintf %str %rd1))))

(defun emit-header ()
  (format t ".version 7.0~%")
  (format t ".target sm_50~%")
  (format t ".address_size 64~%~%"))

;; TODO: Need to update this to use the new list based IR (instead of strings)
(defmacro for ((var &key from to (by 1)) &body body)
  "Generate PTX assembly code for a loop."
  (let* ((loop-label (gensym "loop_start"))
         (end-label (gensym "loop_end")))
    `(tagbody
	(.reg ,(format nil ".u32 ~a" (string-downcase (string var))))
	(.reg ".u32 %max")
	(.reg ".u32 %index")
	(.reg ".pred %p")
	(%mov.u32 ,var ,from)
	(%mov.u32 %max ,to)
	,loop-label
	(%setp.ge.u32 %p ,var %max)
	,(format nil "    @%p bra ~a;" end-label)
	,@body
	(%add.u32 ,var ,var ,by)
	(%bra ,loop-label)
	,end-label)))

(defun translate-args (args)
  (let (result)
    (dolist (arg args)
      ;; set the type of the symbol
      (setf (get (first arg) 'type) (second arg))
      (push (format nil ".reg .b32 ~(~a~)" (first arg)) result))
    (format nil "(~a)" (string-join (nreverse result) ", "))))

;; Define a device function with parameters and a return value
;; TODO: Device function rguments and return value are yet to be implemented
(defmacro defdevice (name args &body body)
  "Define a device function with the given name, arguments and body."
  `(%func ,name ,(translate-args args) (progn ,@body)))

(defmacro defkernel (name args &body body)
  "Define a kernel with the given name, arguments and body."
  `(%entry ,name ,(translate-args args) (progn ,@body)))

(defdevice func ((n int))
  (for (i :from 0 :to n)
    (printf "%d" i)))

(defkernel main ()
  (func 42))

(for (i :from 0 :to 10)
     "mul.lo.u32 %index, %i, 4"
     "st.local.u32 [array + %index], %i")

;; we eventually want to get this working
(for (i :from 0 :to 10)
     (printf "%d" i))

(defun macroexpand-all (form &optional env)
  "Recursively expand all macros in the given form."
  (cond
    ;; If the form is not a list, it's a literal (like a number or symbol), return it as-is
    ((atom form) form)

    ;; If the form is a macro call, expand it and recursively expand the result
    ((macro-function (car form) env)
     (macroexpand-all (macroexpand form env) env))

    ;; Otherwise, recursively expand each element in the list
    (t (cons (macroexpand-all (car form) env)
             (mapcar #'(lambda (x) (macroexpand-all x env))
                     (cdr form))))))

(defun string-join (strings &optional (separator " "))
  "Concatenates a list of strings with the specified separator."
  (with-output-to-string (out)
      (loop for (s . rest) on strings
	    do (write-string s out)
	    unless (null rest)
	      do (write-string separator out))))

(defun indent (out indent-level colon? atsign?)
  (declare (ignore colon? atsign?))
  (let ((indent (make-string (* 2 indent-level) :initial-element #\Space)))
    (format out "~a" indent)))

(defun convert-progn (body indent)
  (with-output-to-string (out)
    (format out "{")
    (dolist (form body)
      (format out "~%~/indent/~a" (1+ indent) (convert-to-ptx form (1+ indent))))
    (format out "~%~/indent/}" indent)))

(defun convert-tagbody (body indent)
  (with-output-to-string (out)
    (dolist (form body)
      (if (symbolp form)
	  (format out "~a: " (string-downcase (string form)))
	  (format out "~a~%~/indent/" (convert-to-ptx form (1+ indent)) indent)))))

(defun convert-function (kind args name body indent)
  (let ((intro (case kind (%entry ".entry") (%func ".func"))))
    (with-output-to-string (out)
      (format out "~a ~(~a~) ~a " intro args name)
      (format out (convert-to-ptx body indent)))))

(defun convert-instruction (form indent)
  (format nil "~a;"
	  (string-join (list (subseq (string-downcase (string (car form))) 1)
			     (string-join (mapcar (lambda (subform)
						    (convert-to-ptx subform indent))
						  (cdr form)) ", ")))))

(defun convert-funcall (function args indent)
  (with-output-to-string (out)
    (let (params)
      (dolist (arg args)
	(let ((param (string-downcase (string (gensym "param")))))
	  (push param params)
	  (format out ".param .b32 ~(~a~);~%~/indent/" param indent)
	  (format out "st.param.b32 [~(~a~)], ~a;~%~/indent/" param arg indent)))
      (format out "call ~(~a~), (~a);" function (string-join (nreverse params) ", ")))))

(defun convert-to-ptx (form &optional (indent 0))
  "Convert a Lisp representation of PTX code into PTX assembly code."
  (cond
    ;; If the form is a list starting with PROGN, treat it as a block
    ((and (consp form) (eq (car form) 'progn))
     (convert-progn (cdr form) indent))

    ;; Convert tagbody
    ((and (consp form) (eq (car form) 'tagbody))
     (convert-tagbody (cdr form) indent))

    ;; Convert kernel and function definitions
    ((and (consp form) (member (car form) '(%entry %func)))
     (convert-function (nth 0 form) (nth 1 form) (nth 2 form) (nth 3 form) indent))

    ;; Convert .GLOBAL, .LOCAL, .REG, etc., as strings
    ((and (consp form) (member (car form) '(.global .local .reg .param)))
     (format nil "~a ~a;" (string-downcase (string (first form))) (second form)))

    ;; Handle individual PTX instructions, converting sub-expressions recursively.
    ((and (consp form) (and (symbolp (first form)) (eq (char (symbol-name (first form)) 0) #\%)))
     (convert-instruction form indent))

    ((consp form)
     (convert-funcall (first form) (rest form) indent))

    ;; Convert symbols and numbers directly to strings, ensuring symbols are lowercase
    ((symbolp form) (string-downcase (symbol-name form)))

    ;; For all other literals, convert to string as-is
    (t (princ-to-string form))))
