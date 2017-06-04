# Dependency installation

1. Install [quicklisp](https://www.quicklisp.org/beta/)
2. Install R (`dev-lang/R` in Gentoo)
   (needed for `libRmath.so` used by `mgl-gpr`)
2. Install [mgl-gpr](https://github.com/melisgl/mgl-gpr) in `local-projects`:
```
git clone https://github.com/melisgl/mgl-gpr.git quicklisp/local-projects/mgl-gpr
```

# Starting

1. Run common lisp interpreter
2. Execute `(load "main.lisp")`
3. Run `(run "ws://192.168.0.12:64645/")`

The other method is to simply execute the main file as in command `sbcl --load main.lisp`.
