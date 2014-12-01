#lang racket/gui
;;; table-panel.ss renamed to table-panel/main.rkt
;;; Copyright(c) 2008, M. Douglas Williams
;;;
;;; This library is free software; you can redistribute it and/or 
;;; modify it under the terms of the GNU Lesser General Public 
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, 
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA.
;;;
;;; -------------------------------------------------------------------
;;;
;;; Version  Date      Description
;;; 1.0.0    11/23/08  Initial release. (Doug Williams)
;;; 1.0.1    11/29/14  Moved from PLaneT to github & Racket 6.1.1.5. (Stephen De Gabrielle)
;;;                    renamed table-panel/main.rkt - no code changes see to be required
;;;

(require srfi/43)

(provide (all-defined-out))

;;; -----------------------------------------------------------------------------
;;;                                Table Panel
;;; -----------------------------------------------------------------------------
;;; A table panel is a panel that aligns its children in a grid where each row
;;; and column are independently sized.
(define table-panel%
  (class panel%
    
    ;; --------------------------------------------------------------------------
    ;;                           Initializations
    ;; --------------------------------------------------------------------------
    
    ;; parent : (or/c (is-a?/c frame%) (is-a?/c dialog%)
    ;;                (is-a?/c panel%) (is-a?/c pane%))
    (init parent)
    (unless (or (is-a? parent frame%) (is-a? parent dialog%)
                (is-a? parent panel%) (is-a? parent pane%))
      (error (format "initialization for table-panel%: expected argument that is an instance of frame%, dialog%, panel%, or pane% for required initialization parent, given ~a"
                     parent)))
    
    ;; dimensions : (list/c exact-positive-integer? exact-positive-integer?)
    (init ((dimensions-init dimensions) '(1 1)))
    (unless (and (list? dimensions-init)
                 (= (length dimensions-init) 2)
                 (exact-positive-integer? (first dimensions-init))
                 (exact-positive-integer? (second dimensions-init)))
      (error (format "initialization for table-panel%: expected argument that is a list of two exact positive integers for initialization dimensions, given ~a"
                     dimensions-init)))
    
    ;; (major-axis-init major-axis) : (symbols row column) = 'row
    (init ((major-axis-init major-axis) 'row))
    (unless (memq major-axis-init '(row column))
      (error "initialization for table-panel%: expected argument that is one of 'row or 'column for initialization major-axis, given ~a"
             major-axis-init))
    
    ;; (column-stretchability-init column-stretchability)
    ;;   : (one-of/c #t 'any 'every #f)
    (init ((column-stretchability-init column-stretchability) 'any))
    (unless (memq column-stretchability-init '(#t any every #f))
      (error "initialization for table-panel%: expected argument that is one of #f, 'any, 'every or #f for initialization column-stretchability, given ~a"
             column-stretchability-init))
    
    ;; (row-stretchability-init row-stretchability)
    ;;   : (one-of/c #t 'any 'every #f)
    (init ((row-stretchability-init row-stretchability) 'any))
    (unless (memq row-stretchability-init '(#t any every #f))
      (error "initialization for table-panel%: expected argument that is one of #f, 'any, 'every or #f for initialization row-stretchability, given ~a"
             row-stretchability-init))
    
    (super-instantiate (parent))
    
    ;; --------------------------------------------------------------------------
    ;;                          Private Variables
    ;; --------------------------------------------------------------------------
    
    (define major-axis major-axis-init)
    (define n-rows (first dimensions-init))
    (define n-columns (second dimensions-init))
    (define column-stretchability column-stretchability-init)
    (define row-stretchability row-stretchability-init)
    
    ;; --------------------------------------------------------------------------
    ;;                   Inherited Methods (from panel%)
    ;; --------------------------------------------------------------------------
    
    (inherit border)
    (inherit spacing)
    (inherit get-alignment)
    
    ;; --------------------------------------------------------------------------
    ;;                            Public Methods
    ;; --------------------------------------------------------------------------
    ;; These methods get and set the appropriate external variables.
    
    ;; (get-dimensions) -> exact-positive-integer? exact-positive-integer?
    ;; Returns the dimensions as two values, number of columns and number of
    ;; rows.
    (define/public (get-dimensions)
      (values n-rows n-columns))
    
    ;; (set-dimensions n-columns-value n-rows-value) -> any
    ;; Set the dimensions.
    (define/public (set-dimensions n-columns-value n-rows-value)
      (unless (and (exact-positive-integer? n-columns-value)
                   (exact-positive-integer? n-rows-value))
        (error 'set-dimensions
               "expects two arguments that are both exact positive integers, given ~a and ~a"
               n-columns-value n-rows-value))
      (set! n-columns n-columns-value)
      (set! n-rows n-rows-value))
    
    ;; (get-major-axis) -> (one-of/c 'row 'column)
    ;; Returns the major axis.
    (define/public (get-major-axis)
      major-axis)
    
    ;; (set-major-axis major-axis-value) -> any
    ;;   major-axis-value : (one-of/c 'row 'column)
    ;; Sets the major axis.
    (define/public (set-major-axis major-axis-value)
      (unless (memq major-axis-value '(row column))
        (error 'set-major-axis
               "expected argument that is one of 'row or 'column, given ~a"
               major-axis-value))
      (set! major-axis major-axis-value))
   
    ;; (get-column-stretchability) -> (one-of/c #t 'any 'every #f)
    ;; Returns the column stretchability.
    (define/public (get-column-stretchability)
      column-stretchability)
    
    ;; (set-column-stratchability value) -> any
    ;;   column-stretchability-value : (one-of/c #t 'any 'every #f)
    ;; Sets the column stretchability.
    (define/public (set-column-stretchability column-stretchability-value)
      (unless (memq column-stretchability-value '(#t any every #f))
        (error 'set-column-stretchability
               "expected argument that is one of #t, 'any, 'every, or #t, given ~a"
               column-stretchability-value))
      (set! column-stretchability column-stretchability-value))
    
    ;; (get-row-stretchability) -> (one-of/c #t 'any 'every #f)
    ;; Returns the row stretchability.
    (define/public (get-row-stretchability)
      row-stretchability)
    
    ;; (set-row-stretchability row-stretchability-value) -> any
    ;;   row-stretchability-value : (one-of/c #t 'any 'every #f)
    ;; Sets the row stretchability.
    (define/public (set-row-stretchability row-stretchability-value)
      (unless (memq row-stretchability-value '(#t any every #f))
        (error 'set-row-stretchability
               "expected argument that is one of #t, 'any, 'every, or #t, given ~a"
               row-stretchability-value))
      (set! row-stretchability row-stretchability-value))
    
    ;; --------------------------------------------------------------------------
    ;;                          Private Functions
    ;; --------------------------------------------------------------------------
    ;; These functions are used internally as part of the implementation of
    ;; computing container size and child placement.
    
    ;; (dereference-index i) -> exact-positive-integer? exact-positive-integer?
    ;;   i : exact-positive-integer?
    ;; Returns the row and column for an index as multiple values based on the
    ;; major axis.
    (define (dereference-index i)
      (case major-axis
        ((row)
         (let-values (((row column) (quotient/remainder i n-columns)))
           (values row column)))
        ((column)
         (let-values (((column row) (quotient/remainder i n-rows)))
           (values row column)))))
    
    ;; (compute-sizes info) -> (vectorof (integer-in 0 10000))
    ;;                         (vectorof (integer-in 0 10000))
    ;;                         (vectorof boolean?)
    ;;                         (vectorof boolean?)
    ;;   info : (listof (list/c (integer-in 0 10000)
    ;;                          (integer-in 0 10000)
    ;;                          any/c
    ;;                          any/c))
    ;; Computes the column widths and row heights across all of the cells in the
    ;; table panel and returns four values: the number of columns, the number of
    ;; rows, a vector of column-widths, and a vector of row heights.
    (define (compute-sizes info)
      (let* ((column-widths (make-vector n-columns 0))
             (row-heights (make-vector n-rows 0))
             (column-stretchabilities?
              (make-vector n-columns
                           (if (memq column-stretchability '(#t 'every))
                               #t #f)))
             (row-stretchabilities?
              (make-vector n-rows
                           (if (memq row-stretchability '(#t 'every))
                               #t #f))))
        (for ((child-info (in-list info))
              (i (in-naturals)))
          (let-values (((row column) (dereference-index i)))
            (let ((child-width (first child-info))
                  (child-height (second child-info))
                  (child-horiz-stretch? (third child-info))
                  (child-vert-stretch? (fourth child-info)))
              (vector-set! column-widths column
                           (max child-width (vector-ref column-widths column)))
              (vector-set! row-heights row
                           (max child-height (vector-ref row-heights row)))
              (case column-stretchability
                ((any)
                 (vector-set! column-stretchabilities? column
                              (or child-horiz-stretch?
                                  (vector-ref column-stretchabilities? column))))
                ((every)
                 (vector-set! column-stretchabilities? column
                              (and child-horiz-stretch?
                                  (vector-ref column-stretchabilities? column)))))
              (case row-stretchability
                ((any)
                 (vector-set! row-stretchabilities? row
                              (or child-vert-stretch?
                                  (vector-ref row-stretchabilities? row))))
                ((every)
                 (vector-set! row-stretchabilities? row
                              (and child-vert-stretch?
                                   (vector-ref row-stretchabilities? row))))))))
        (values column-widths row-heights
                column-stretchabilities? row-stretchabilities?)))
    
    ;; (adjust-sizes column-widths row-heights
    ;;               column-stretchabilities? row-stretchabilities?
    ;;               width height)
    ;; -> any
    ;;   column-widths : (vectorof (integer-in 0 10000))
    ;;   row-widths : (vectorof (integer-in 0 10000))
    ;;   column-stretchabilities? : (vectorof boolean?)
    ;;   row-stretchabilities? (vectorof boolean?)
    ;;   width : (integer-in 0 10000)
    ;;   height : (integer-in 0 10000)
    ;; Adjust the column widths and row heights based on the stretchability of
    ;; the rows and columns.
    (define (adjust-sizes column-widths row-heights
                          column-stretchabilities? row-stretchabilities?
                          width height)
      (let* ((total-column-width
              (vector-fold
               (lambda (i total-width width)
                 (+ total-width width))
               0 column-widths))
             (total-row-height
              (vector-fold
               (lambda (i total-height height)
                 (+ total-height height))
               0 row-heights))
             (total-width
              (+ total-column-width (border) (* (- n-columns 1) (spacing))))
             (total-height
              (+ total-row-height (border) (* (- n-rows 1) (spacing))))
             (delta-width (- width total-width))
             (delta-height (- height total-height)))
        (when (> delta-width 0)
          (let ((total-column-stretchable-width
                 (vector-fold
                  (lambda (i total-stretchable-width stretchable? width)
                    (if stretchable?
                        (+ total-stretchable-width width)
                        total-stretchable-width))
                  0 column-stretchabilities? column-widths)))
            (for ((i (in-naturals))
                  (stretchable? (in-vector column-stretchabilities?))
                  (width (in-vector column-widths)))
              (when (and stretchable? (> total-column-stretchable-width 0))
                (let* ((ratio (/ width total-column-stretchable-width))
                       (quota (round (* ratio delta-width))))
                  (vector-set! column-widths i
                               (+ (vector-ref column-widths i) quota))
                  (set! total-column-stretchable-width
                        (- total-column-stretchable-width width))
                  (set! delta-width (- delta-width quota)))))))
        (when (> delta-height 0)
          (let ((total-row-stretchable-height
                 (vector-fold
                  (lambda (i total-stretchable-height stretchable? height)
                    (if stretchable?
                        (+ total-stretchable-height height)
                        total-stretchable-height))
                  0 row-stretchabilities? row-heights)))
            (for ((i (in-naturals))
                  (stretchable? (in-vector row-stretchabilities?))
                  (height (in-vector row-heights)))
              (when (and stretchable? (> total-row-stretchable-height 0))
                (let* ((ratio (/ height total-row-stretchable-height))
                       (quota (round (* ratio delta-height))))
                  (vector-set! row-heights i
                               (+ (vector-ref row-heights i) quota))
                  (set! total-row-stretchable-height
                        (- total-row-stretchable-height height))
                  (set! delta-height (- delta-height quota)))))))
        ))
    
    ;; (compute-offsets column-widths row-heights)
    ;; -> (vectorof (integer-in 0 10000))
    ;;    (vectorof (integer-in 0 10000))
    ;;   column-widths : (vectorof (integer-in 0 10000))
    ;;   row-heights : (vectorof (integer-in 0 10000))
    ;; Computes the row and column offsets and returns them as vectors.
    (define (compute-offsets column-widths row-heights)
      (values
       (vector-unfold
        (lambda (i x)
          (values x (+ x (vector-ref column-widths i) (spacing))))
        (vector-length column-widths) (border))
       (vector-unfold
        (lambda (i x)
          (values x (+ x (vector-ref row-heights i) (spacing))))
        (vector-length row-heights) (border))))
    
    ;; --------------------------------------------------------------------------
    ;;                                Container Size
    ;;---------------------------------------------------------------------------
    
    ;; (container-size info) -> (integer-in 0 10000) (integer-in 0 10000)
    ;;   info : (listof (list/c (integer-in 0 10000)
    ;;                          (integer-in 0 10000)
    ;;                          any/c
    ;;                          any/c))
    ;; Compute the minimum width and height of the tabel panel based on the
    ;; children sizes.
    (define/override (container-size info)
      (let-values (((column-widths row-heights
                     column-stretchabilities? row-stretchabilities?)
                    (compute-sizes info)))
        (values
         (vector-fold
          (lambda (i total-width column-width)
            (+ total-width column-width (spacing)))
          (* (border) 2) column-widths)
         (vector-fold
          (lambda (i total-height row-height)
            (+ total-height row-height (spacing)))
          (* (border) 2) row-heights))))
    
    ;; --------------------------------------------------------------------------
    ;;                           Child Placement
    ;; --------------------------------------------------------------------------
    
    ;; (place-child column-offset row-offset child-info)
    ;;  -> (listof (list/c (integer-in 0 10000)
    ;;                     (integer-in 0 10000)
    ;;                     (integer-in 0 10000)
    ;;                     (integer-in 0 10000)))
    ;;   column-width : (integer-in 0 10000)
    ;;   row-height : (integer-in 0 10000)
    ;;   column-offset : (integer-in 0 10000)
    ;;   row-offset : (integer-in 0 10000)
    ;;   child-info : (list/c (integer-in 0 10000)
    ;;                        (integer-in 0 10000)
    ;;                        any/c
    ;;                        any/c)
    ;; Compute the placement a child.
    (define (place-child
             column-width row-height
             column-stretchable? row-stretchable?
             column-offset row-offset
             child-info)
      (let* ((child-width (first child-info))
             (child-height (second child-info))
             (horiz-stretch? (third child-info))
             (vert-stretch? (fourth child-info))
             (delta-width (- column-width child-width))
             (delta-height (- row-height child-height)))
        (unless (= delta-width 0)
          (if horiz-stretch?
              (set! child-width column-width)
              (let-values (((horiz-alignment vert-alignment)
                            (get-alignment)))
                (case horiz-alignment
                  ((center)
                   (set! column-offset
                         (+ column-offset (quotient delta-width 2))))
                  ((right)
                   (set! column-offset
                         (+ column-offset delta-width)))))))
        (unless (= delta-height 0)
          (if vert-stretch?
              (set! child-height row-height)
              (let-values (((horiz-alignent vert-alignment)
                            (get-alignment)))
                (case vert-alignment
                  ((center)
                   (set! row-offset
                         (+ row-offset (quotient delta-height 2))))
                  ((bottom)
                   (set! row-offset
                         (+ row-offset delta-height)))))))
        (list column-offset row-offset child-width child-height)))
    
    ;; (place-children info width height)
    ;;  -> (listof (list/c (integer-in 0 10000)
    ;;                     (integer-in 0 10000)
    ;;                     (integer-in 0 10000)
    ;;                     (integer-in 0 10000)))
    ;;   info : (listof (list/c (integer-in 0 10000)
    ;;                          (integer-in 0 10000)
    ;;                          any/c
    ;;                          any/c))
    ;;   width : (integer-in 0 10000)
    ;;   height : (integer-in 0 10000)
    ;; Compute placement of all children.
    (define/override (place-children info width height)
      (let-values (((column-widths row-heights
                     column-stretchabilities? row-stretchabilities?)
                    (compute-sizes info)))
        (adjust-sizes column-widths row-heights
                      column-stretchabilities? row-stretchabilities?
                      width height)
        (let-values (((column-offsets row-offsets)
                      (compute-offsets column-widths row-heights)))
          (for/list ((child-info (in-list info))
                     (i (in-naturals)))
            (let-values (((row column) (dereference-index i)))
              (place-child
               (vector-ref column-widths column)
               (vector-ref row-heights row)
               (vector-ref column-stretchabilities? column)
               (vector-ref row-stretchabilities? row)
               (vector-ref column-offsets column)
               (vector-ref row-offsets row)
               child-info))))))
    
    )
  )

