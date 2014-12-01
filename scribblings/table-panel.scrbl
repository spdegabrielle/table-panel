#lang scribble/doc

@(require scribble/manual
          scribblings/icons)

@require[@for-label[table-panel
                    racket/gui/base]]

@title[#:tag "top"]{Table Panel}

@(author+email @tt{M. Douglas Williams} "doug@cognidrome.org")

This library provides a @racket[table-panel%] that specializes the racket/gui/base @racket[panel%] class to provide a panel that aligns its children to a grid.  A simple demonstration program is also provided.

This is partially based on the Simple Grid Layout recipe by Danny Yoo on the Schematics Cookbook web site.

http://schemecookbook.org/Cookbook/SimpleGridLayout


Everything in this library is exported by a single module:

@defmodule[(planet williams/table-panel/table-panel)]

@table-of-contents[]

@section{Theory of Operation}

@(margin-note finger "This section assumes you are familiar with the " @racket[panel%] " class.")

A @deftech{table panel} provides a panel that aligns its children on a grid.  This is implemented by a new class @racket[table-panel%] that specializes the @racket[panel%] class.

Currently, the absolute dimensions of the grid are specified, as well as the major axis.

There is a fair amount of flexibility provided in terms of specifying how column widths and row heights can be adjusted (stretched) in response to resizing of the panel.  This can be specified absolutely---that is, columns (or rows) can always be stretched or never be stretched, or based on the characteristics of the cells---that is, columns (or rows) can be stretched if any, or all, or its cells can be stretched.

The table panel's behavior is specified by the @racketfont{container-size} and @racketfont{place-children} methods.  The @racketfont{container-size} method computes the size of the grid based on the minimum sizes of each of the grids columns and rows (as well as any border and spacing specified).  The @racketfont{place-children} method computes the column widths and row heights based on the actual space available, adjusting them as necessary, and places each child in the grid based on the alignment option of the grid, the child's location (row and column) in the grid, and the child's stretchability (horizontal and vertical).

@section{Interface}

@defclass[table-panel% panel% ()]{
                                  
A @racket[table-panel%] object is a specialized @racket[panel%] object that aligns its children on a grid.
  
Most of the following description is from the GUI: PLT Graphics Toolkit reference manual with additions as noted for @racket[table-panel%].  Changes that are specific to the @racket[table-panel%] class are in bold face.

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'border 'deleted)) null]
                 [vert-margin (integer-in 0 10000) 0]
                 [horiz-margin (integer-in 0 10000) 0]
                 [border (integer-in 0 10000) 0]
                 [spacing (integer-in 0 10000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(center center)]
                 [min-width (integer-in 0 10000) graphical-minimum-width]
                 [min-height (integer-in 0 10000) graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t]
                 [dimensions (list/c exact-positive-integer?
                                     exact-positive-integer?)
                             '(1 1)]
                 [major-axis (one-of/c 'row 'column) 'row]
                 [column-stretchability (one-of/c #t 'any 'every #f) 'any]
                 [row-stretchability (one-of/c #t 'any 'every #f) 'any])]{

If the @racket['border] style is specified, the window is created with a thin border (only in this case, the client size of the panel may be less than its total size). If @racket[style] includes @racket['deleted], then the panel is created as hidden, and it does not affect its parent’s geometry; the panel can be made active later by calling @racket[parent]’s @racket[add-child] method.

For information about the @racket[enabled] argument, see @racket[window<%>]. For information about the @racket[horiz-margin] and @racket[vert-margin] arguments, see @racket[subarea<%>]. For information about the @racket[border], @racket[spacing], and @racket[alignment] arguments, see @racket[area-container<%>]. For information about the @racket[min-width], @racket[min-height], @racket[stretchable-width], and @racket[stretchable-height] arguments, see @racket[area<%>].

The @racket[dimensions] argument specifies the number of rows and columns in the grid.  There should be @math{rows × columns} children of the panel.  The default is @racket['(1 1)].

The @racket[major-axis] argument specifies the major axis for in the grid.  This specifies the order in which the children are placed in the grid.  If @racket[major-axis] is @racket['row], then all of the columns in a row are placed before moving to the next row.  If @racket[major-axis] is @racket['column], then all of the rows for a column are placed before moving to the next column.  The default is @racket['row].  

For example, if @racket[dimensions] is @racket['(2 2)], then there should be four children, say, @math{child_0}, @math{child_1}, @math{child_2}, and @math{child_3}.  If @racket[major-axis] is @racket['row], then @math{child_0} is placed in @math{row_0} @math{column_0}---the upper left cell, @math{child_1} is placed in @math{row_0} @math{column_1}---the upper right cell, @math{child_2} is placed in @math{row_1} @math{column_0}---the lower left cell, and @math{child_2} is placed in @math{row_1} @math{column_1}---the lower right cell.  If @racket[major-axis] is @racket['column], then @math{child_0} is placed in @math{row_0} @math{column_0}---the upper left cell, @math{child_1} is placed in @math{row_1} @math{column_0}---the lower left cell, @math{child_2} is placed in @math{row_0} @math{column_1}---the upper right cell, and @math{child_2} is placed in @math{row_1} @math{column_1}---the lower right cell.

The @racket[column-stretchability] and @racket[row-stretchability] arguments specify how column widths and row heights can be adjusted when additional space (above the minimum sizes) is available:

@itemize{
  @item{@racket[#t]---Column widths (or row heights) are always stretchable.}
  @item{@racket['any]---The width of a column (or the height of a row) is stretchable if any of its rows is stretchable horizontally (or any of its columns are stretchable vertically).  That is, a column or row can stretch if any of its cells can take advantage of the extra space.}
  @item{@racket['every]---The width of a column (or the height of a row) is stretchable if all of its rows is stretchable horizontally (or all of its columns are stretchable vertically).  That is, a column or row can stretch if all of its cells can take advantage of the extra space.}
  @item{@racket[#f]---Column widths (or row heights) are never stretchable.}
  }
}
                                                                         
@defmethod[(get-dimensions) (values exact-positive-integer? exact-positive-integer?)]{
Returns the dimensions of the table panel object as two values, the number of rows and the number of columns.}

@defmethod[(set-dimensions (n-rows exact-positive-integer)
                           (n-columns exact-positive-integer)) any]{
Sets the dimensions of the table panel object to @racket[n-rows] and @racket[n-columns].}
                                                                   
@defmethod[(get-major-axis) (one-of/c 'row 'column)]{
Returns the major axis for the table panel object.}

@defmethod[(set-major-axis (major-axis (one-of/c 'row 'column))) any]{
Sets  the major axis for the table panel object to @racket[major-axis].}

@defmethod[(get-column-stretchability) (one-of/c #t 'any 'every #f)]{
Returns the column stretchability for the table panel object.}

@defmethod[(set-column-stretchability (stretchability (one-of/c #t 'any 'every #f))) any]{
Sets the column stretchability for the table panel object to @racket[stretchability].}

@defmethod[(get-row-stretchability) (one-of/c #t 'any 'every #f)]{
Returns the row stretchability for the table panel object.}

@defmethod[(set-row-stretchability (stretchability (one-of/c #t 'any 'every #f))) any]{
Sets the row stretchability for the table panel object to @racket[stretchability].}

}

@section{Example Table Panel}

This section contains an example that uses table panel examples.

@racketmod[
racket/gui

(code:comment " Table panel example.")
(code:comment " This example displays a table panel containing four more")
(code:comment " table panels anddemonstrates some column and row stretch-")
(code:comment " ability options.")

(require (planet williams/table-panel/table-panel))

(code:comment " The top-level frame")
(define frame
  (instantiate frame%
    ("Test")))

(code:comment " A 2 x 2 table panel")
(define table-panel
  (instantiate table-panel%
    (frame)
    (alignment '(center center))
    (dimensions '(2 2))))

(code:comment " Fill each cell with a table panel displaying a simulated keypad.")
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

(code:comment " Show the top-level frame.")
(send frame show #t)
]

The following image shows the output as originally sized by PLT Racket based on the minimum sizes of the elements.

@image["images/example-1.png"]

The next image shows the output after resizing the frame.  The upper left panel specifies both row and column stretchability, the upper right panel specifies only column stretchability, the lower left panel specifies only row stretchability, and the lower right panel specified neither column nor row stretchability.

@image["images/example-2.png"]

Note that the outer @math{2 × 2} table panel specifies the default column and row stretchability of @racket['any], which, in this case, allows its columns and rows to stretch because (at least one of) each of its columns and rows can stretch.

@section{Issues and To Do}

It isn't really necessary to specify both the number of rows and the number of columns.  All we need to know is the major axis and the dimensionality of the minor axis.  The dimensionality of the major axis can be determined using the number of children and the dimensionality of the minor axis---@racket[(ceiling (/ (length children) n-minor-axis))].  We could allow one of the dimensions to be specified by a placeholder.  For example, @racketfont{(dimensions '(* 3))} would specify a row major table with each row having three columns.  Similar changes would have to be made for @racket[get-dimensions] and @racket[set-dimensions].  We would also have to add some error checking to make sure the major axis is compatible with the dimensions.