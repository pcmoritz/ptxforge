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
(defmacro call (function &rest args)
  `(progn
     (.param .b64 param0)
     (.param .b64 param1)
     (.param .b32 retval)
     (st.param.b64 "[param0+0]" ,(nth 0 args))
     (st.param.u64 "[param1+0]" ,(nth 1 args))
     (call.uni "(retval)" ,function "(param0, param1)")))

;; TODO: Add some more error checking here. Also, this should be made a lot more generic
(defmacro printf (fmt &rest args)
  (let* ((depot (string (gensym "local_depot"))))
    `(progn
       (.global ".align 1 .b8 $str[4] = {37, 100, 10}")
       (.local ".align 8 .b8 " ,depot)
       (.reg .b64 %SP)
       (.reg .b64 %SPL)
       (.reg .b32 %r)
       (.reg .b64 %rd<3>)
       (.reg .u64 %str)
       (mov.u64 %SPL ,depot)
       (cvta.local.u64 %SP %SPL)
       (add.u64 %rd1 %SP 0)
       (add.u64 %rd2 %SPL 0)
       (mov.u32 %r ,(nth 0 args))
       (cvta.global.u64 %str $str)
       (call 'vprintf %str %rd1))))

(defun emit-header ()
  (format t ".version 7.0~%")
  (format t ".target sm_50~%")
  (format t ".address_size 64~%~%"))

(defmacro for ((var &key from to (by 1)) &body body)
  "Generate PTX assembly code for a loop."
  (let* ((start from)
         (end to)
         (step by)
         (loop-label (string (gensym "loop_start")))
         (end-label (string (gensym "loop_end")))
         (ptx-body (mapcar (lambda (form)
                             (format nil "~a;" form))
                           body)))
    `(progn
       (format t ".entry main~%")
       (format t "{~%")
       (format t "    .reg .u32 ~a;~%" ',var)
       (format t "    .reg .u32 %max;~%")
       (format t "    .reg .u32 %index;~%")
       (format t "    .reg .pred %p;")
       (format t "    .local .u32 array[~d];~%~%" (1+ ,end))
       (format t "    mov.u32 ~a, ~d;~%" ',var ,start)
       (format t "    mov.u32 %max, ~d;~%~%" ,end)
       (format t "~a:~%" ,loop-label)
       (format t "    setp.ge.u32 %p, ~a, %max;~%" ',var)
       (format t "    @%p bra ~a;~%" ,end-label)
       ,@(mapcar (lambda (form)
                   `(format t "    ~a~%" ,form))
                 ptx-body)
       (format t "    add.u32 ~a, ~a, ~d;~%" ',var ',var ,step)
       (format t "    bra ~a;~%" ,loop-label)
       (format t "~a:~%" ,end-label)
       (format t "    ret;~%")
       (format t "}~%"))))

;; Define a device function with parameters and a return value
;; TODO: Device function rguments and return value are yet to be implemented
(defmacro defdevice (name args &body body)
  "Defines a device function with the given name, arguments and body."
  `(".entry" "{" "ret;" "}"))

(defdevice main ()
  (for (i :from 0 :to 10)
       (printf "%d" i)))

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
  (reduce (lambda (x y) (concatenate 'string x separator y)) strings))

(defun convert-to-ptx (form &optional (indent-level 0))
  "Convert a Lisp representation of PTX code into PTX assembly code."
  (let ((indent (make-string (* 2 indent-level) :initial-element #\Space)))
    (cond
      ;; If the form is a list starting with PROGN, treat it as a block and recursively convert.
      ((and (consp form) (eq (car form) 'progn))
       (format nil "{~%~a~%~a}"
               (string-join (mapcar (lambda (subform)
                                      (convert-to-ptx subform (1+ indent-level)))
                                    (cdr form))
			    (format nil ";~%~a" indent))
               indent))

      ;; Convert .GLOBAL, .LOCAL, .REG, etc., as strings
      ((and (consp form) (stringp (car form)))
       (format nil "~a" (string-join (mapcar #'princ-to-string form) " ")))

      ;; Handle individual PTX instructions, converting sub-expressions recursively.
      ((consp form)
       (format nil "~a ~a"
               (string-downcase (princ-to-string (car form)))
               (string-join (mapcar (lambda (subform)
                                      (convert-to-ptx subform indent-level))
                                    (cdr form)) " ")))

      ;; Convert symbols and numbers directly to strings, ensuring symbols are lowercase
      ((symbolp form) (string-downcase (symbol-name form)))

      ;; For all other literals, convert to string as-is
      (t (princ-to-string form)))))
