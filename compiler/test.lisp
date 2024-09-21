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

;; TODO: Add some more error checking here
(defmacro printf (fmt &rest args)
  (let* ((depot (string (gensym "local_depot"))))

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

;; define a device function with parameters and a return value
;; (defmacro defdevice

(for (i :from 0 :to 10)
     "mul.lo.u32 %index, %i, 4"
     "st.local.u32 [array + %index], %i")

;; we eventually want to get this working
(for (i :from 0 :to 10)
     (printf "%d" i))