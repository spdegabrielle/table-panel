#lang racket/gui

;;; Table panel example.
;;; This example displays a table panel containing four more table panels and
;;; demonstrates some column and row stretchability options.

(require "main.rkt")

;;; The top-level frame
(define frame
  (instantiate frame%
    ("Test")))

;;; A 2 x 2 table panel
(define table-panel
  (instantiate table-panel%
    (frame)
    (alignment '(center center))
    (dimensions '(2 2))))

;;; Fill each cell with a table panel displaying a simulated keypad.
(for ((i (in-range 4)))
  (let ((child (instantiate table-panel%
                 (table-panel)
                 (style '(border))
                 (dimensions '(4 3))
                 (column-stretchability (if (memq i '(0 1)) #t #f))
                 (row-stretchability (if (memq i '(0 2)) #t #f)))))
    (for ((j '(1 2 3
               4 5 6
               7 8 9
               * 0 \#)))
      (instantiate button%
        ((format "~a" j) child)
        (stretchable-width #t)
        (stretchable-height #t)
        (callback
         (lambda (button event)
           (printf "~a~n" (send button get-label))))))))

;;; Show the top-level frame.
(send frame show #t)

