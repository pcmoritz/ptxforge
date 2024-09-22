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
