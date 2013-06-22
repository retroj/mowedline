
;;; mowedline.el --- elisp utilities for using mowedline

;; This file is part of mowedline.
;; Copyright (C) 2011-2013  John J. Foerch
;;
;; mowedline is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; mowedline is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with mowedline.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides `mowedline-colorize', a utility that converts a
;; propertied Emacs string into a string of mowedline markup, preserving
;; colors.

;;; Code:

(defun mowedline-string-break-by-property (str prop)
  (let ((len (length str))
        (p 0)
        q l)
    (while (setq q (next-single-property-change p prop str))
      (setq l (cons (substring str p q) l))
      (setq p q))
    (unless (= p len)
      (setq l (cons (substring str p len) l)))
    (nreverse l)))

(defun mowedline-colorize (str &optional trim)
  "Translate the string STR into a string representation of a
mowedline markup structure, with `color' groups that preserve the
foreground colors of the faces from the original string."
  (when trim
    (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                         str)
      (setq str (replace-match "" t t str))))
  (if (string= "" str)
      "()"
    (format
     "%S"
     (mapcar
      (lambda (x)
        (let ((face (get-text-property 0 'face x)))
          (if face
              `(color ,(face-foreground face)
                      ,(substring-no-properties x))
            x)))
      (mowedline-string-break-by-property str 'face)))))

(provide 'mowedline)
;;; mowedline.el ends here
