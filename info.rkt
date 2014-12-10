#lang info

(define collection "table-panel")

(define deps '("base"
               "rackunit-lib"
               "gui"
               "srfi-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/table-panel.scrbl" ())))
(define pkg-desc "This library provides a table-panel% class that specializes the panel% class to provide a panel that aligns its children to a grid. A simple demonstration program is also provided. Copyright(c) 2008, M. Douglas Williams.")
(define version "1.0.1")
;?(define pkg-authors '(mdwilliams))
;;
(define compile-omit-paths '("test-table-panel.rkt"))
