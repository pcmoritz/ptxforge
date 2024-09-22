# ptxforge
Higher level abstractions to write PTX code

The idea is that the programmer can specify macros that will expand
into PTX code. Our goal is to get the program

```lisp
(defdevice main ()
  (for (i :from 0 :to 10)
       (printf "%d" i)))
```

working by the end of the hackaton. This can be extended to support
arbitrary PTX instructions like mma instructions, which would make it
feasible to write a matrix multiplication kernel, yet keep the
abstractions flexible enough to be able to fuse arbitrary computations
into the kernel.

The fact that this is implemented with Common Lisp macros makes it
very hackable, the users can define their own primitives and also
really optimize the code down to the PTX / register level.

Using this is currently a little clunky since no compiler driver is
implemented. You need a Common Lisp implementation like sbcl and then
load the definitions in `compiler/test.lisp` and execute

```lisp
(defparameter *ptx* (macroexpand-all '(printf "%d" 42)))
(convert-to-ptx *ptx*)
```

The resulting PTX needs to be put into a .ptx file (with the
appropriate header) and then run

```shell
nvcc --cubin -arch=sm_90a -o test.cubin test.ptx
nvcc -arch=sm_90a -o test runtime/driver.cu -lcuda
```

and execute the executable with `./test`.
