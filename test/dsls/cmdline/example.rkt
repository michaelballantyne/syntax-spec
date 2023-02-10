#lang racket/base

(require "cmdline.rkt" "sugar.rkt" (for-syntax racket/base syntax/parse))

(define (existing-file/p str)
  (if (file-exists? str)
      str
      (raise-user-error "expected path to exisiting file")))

(define/command-line-options
  #:options
  [verbose-mode (switch/o ("-v" "--verbose") "Compile with verbose messages")]
  [profiling-on (switch/o ("-p" "--profile") "Compile with profiling")]
  [optimize-level
   (choice/o #:default 0
             ["--optimize-level" [lvl (int-range/p 0 3)]
                                 "set optimization level to <lvl>" lvl]
             (numbered-flags/f "--o" [0 3] "optimization level"))]
  [output
   (required/o "-o" outfile "the output filename" outfile)]
  [link-flags (list/o ["-l" "--link-flags"] lf "Add a flag <lf> for the linker")]
  #:arguments
  [file-to-compile existing-file/p])
