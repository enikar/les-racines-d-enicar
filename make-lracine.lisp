;; Compile and build an executable to be used from command line
;; Usage: sbcl --script make-lracine.lisp
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(with-compilation-unit
    (:policy '(optimize)
     :override T)
    (load "lracine.lisp"))


(save-lisp-and-die "lracine"
                   :toplevel #'lrac:main
                   :executable T
                   :purify T
                   :compression T)
