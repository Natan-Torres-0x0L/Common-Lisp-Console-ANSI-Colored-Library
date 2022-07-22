(defpackage :colored (:use :common-lisp))

(provide :colored)
(in-package :colored)

(export '(reset color256 rgb256 color rgb style

          *foreground*    *background*

          *black*         *light-gray*
          *red*           *light-red*
          *green*         *light-green*
          *yellow*        *light-yellow*
          *blue*          *light-blue*
          *magenta*       *light-magenta*
          *cyan*          *light-cyan*
          *white*         *light-white*

          *bold*          *no-bold*
          *shadow*        *no-shadow*
          *italic*        *no-italic*
          *underline*     *no-underline*
          *blinking*      *no-blinking*
          *negative*      *no-negative*
          *invisible*     *no-invisible*
          *strikethrough* *no-strikethrough*))


(defconstant *foreground*    38)
(defconstant *background*    48)


(defconstant *black*          0)
(defconstant *red*            1)
(defconstant *green*          2)
(defconstant *yellow*         3)
(defconstant *blue*           4)
(defconstant *magenta*        5)
(defconstant *cyan*           6)
(defconstant *white*          7)
(defconstant *light-gray*     8)
(defconstant *light-red*      9)
(defconstant *light-green*   10)
(defconstant *light-yellow*  11)
(defconstant *light-blue*    12)
(defconstant *light-magenta* 13)
(defconstant *light-cyan*    14)
(defconstant *light-white*   15)

(defconstant *bold*              1)
(defconstant *no-bold*          22)
(defconstant *shadow*            2)
(defconstant *no-shadow*        22)
(defconstant *italic*            3)
(defconstant *no-italic*        23)
(defconstant *underline*         4)
(defconstant *no-underline*     24)
(defconstant *blinking*          5)
(defconstant *no-blinking*      25)
(defconstant *negative*          7)
(defconstant *no-negative*      27)
(defconstant *invisible*         8)
(defconstant *no-invisible*     28)
(defconstant *strikethrough*     9)
(defconstant *no-strikethrough* 29)


(defun reset ()
  (format nil "~c[0m" #\ESC))

(defun colored-color256 (context color256)
  (format nil "~c[~d;5;~dm" #\ESC context color256))

(defun color256 (context color256)
  (colored-color256 context color256))

(defun colored-context (string)
  (case (char string 0)
    (#\# *foreground*) (#\@ *background*) (t nil)))

(defun colored-rgb-values (rgb)
  (if (= (length rgb) 8)
    (let ((context (colored-context rgb))
          (red (parse-integer (subseq rgb 2 4) :radix 16))
          (green (parse-integer (subseq rgb 4 6) :radix 16))
          (blue (parse-integer (subseq rgb 6 8) :radix 16)))
      (values context red green blue))))

(defun rgb256 (rgb256)
  (multiple-value-bind (context red green blue) (colored-rgb-values rgb256)
    (if context
      (if (apply '= (list red green blue))
        (cond
          ((> red 248) 231)
          ((< red 8) 16)
          (t (color256 context (round (+ (* (/ (- red 8) 247.0) 24) 232.0)))))
    ; (else
        (color256 context (+ 16 (* 36 (round (* (/ red 255.0) 5))) (* 6 (round (* (/ green 255.0) 5))) (round (* (/ blue 255.0) 5)))))
      "")))

(defun internal-star-notation (name)
  (string-upcase (format nil "COLORED::*~a*" name)))

(defun colored-string-as-constant (name)
  (let ((name (read-from-string (internal-star-notation name)))) ; intern
    (if (boundp name)
      (symbol-value name))))

(defmacro string-empty (string)
  `(or (null ,string) (zerop (length ,string))))

(defun color (color)
  (if (not (string-empty color))
    (let ((context (colored-context color)))
      (if (and context)
        (let ((color (colored-string-as-constant (subseq color 1))))
          (if color
            (color256 context color)
            ""))))))

(defmacro colored-rgb (context red green blue)
  `(progn (format nil "~c[~d;2;~d;~d;~dm" #\ESC context red green blue)))

(defun rgb (rgb)
  (multiple-value-bind (context red green blue) (colored-rgb-values rgb)
    (if context
      (colored-rgb context red green blue)
      "")))

(defmacro colored-style (style)
  `(progn (format nil "~c[~dm" #\ESC style)))

(defun style (style)
  (let ((style (colored-string-as-constant style)))
    (if style
      (colored-style style)
      "")))
